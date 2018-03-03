package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import java.net.URL
import java.util.zip.ZipInputStream
import java.io.ByteArrayInputStream

import doomWad._

import java.lang.Runtime

//import ps.tricerato.pureimage._

sealed trait Demo
case class Index() extends Demo
case class DoomWadReader() extends Demo
case class Maze() extends Demo
case class Samurai() extends Demo
case class Cycle3467() extends Demo
case class Extras() extends Demo

sealed trait Page
case class Welcome() extends Page
case class Resume() extends Page
case class Demos(demo: Demo) extends Page
case class Contact() extends Page

object Page {
  def link(page: Page) = page match {
    case Welcome() => routes.Application.welcome()
    case Resume() => routes.Application.resume()
    case Demos(Index()) => routes.Application.demos()
    case Demos(DoomWadReader()) => routes.Application.demosDoomWadReader()
    case Demos(Maze()) => routes.Application.demos3DMaze()
    case Demos(Samurai()) => routes.Application.demosSamurai()
    case Demos(Cycle3467()) => routes.Application.demosCycle3467()
    case Demos(Extras()) => routes.Application.demosExtras()
    case Contact() => routes.Application.contact()
  }
}

class Application extends Controller {
  
  def index = Action {
    Ok(views.html.main(None))
  }

  def welcome = Action {
    Ok(views.html.main(Some(Welcome())))
  }

  def resume = Action {
    Ok(views.html.main(Some(Resume())))
  }

  def demos = Action {
    Ok(views.html.main(Some(Demos(Index()))))
  }

  def demosDoomWadReader = Action {
    Ok(views.html.main(Some(Demos(DoomWadReader()))))
  }

  def demos3DMaze = Action {
    Ok(views.html.main(Some(Demos(Maze()))))
  }

  def demosSamurai = Action {
    Ok(views.html.main(Some(Demos(Samurai()))))
  }

  def demosCycle3467 = Action {
    Ok(views.html.main(Some(Demos(Cycle3467()))))
  }

  def demosExtras = Action {
    Ok(views.html.main(Some(Demos(Extras()))))
  }

  def contact = Action {
    Ok(views.html.main(Some(Contact())))
  }
  
  lazy val doomWadParser = new DoomWadParser
  
  lazy val standardDoomAssets = {
    // TODO: access this file by pulling it from our own server, making it very unlikely to be down
    val bytes = getBytesFromURL("http://www.jbserver.com/downloads/games/doom/misc/shareware/doom1.wad.zip")
    val (_, wadBytes) = getLikelyBytesForWad(bytes)
    
    val parseResult = doomWadParser.parseDoomWad(new ByteReader(wadBytes))
    parseResult match {
      case doomWadParser.Success(wad: DoomIWad, _) => new DoomAssets(wad)
      case _ => new DoomAssets(new DoomIWad)
    }
  }
    
  def getBytesFromURL(url: String) = {
    try {
      // fully blocking reading of resource....good enough for now, because not a large number of simultaneous connections are expected
      // this code is ugly because it handles .wad and non zips simultaneously....TODO: make it prettier
      val inputStream = (new URL(url)).openStream
      val bytes = Stream.continually(inputStream.read).takeWhile(_ != -1).map(_.toByte).toArray
      inputStream.close
      bytes
    } catch {
      case ioe: java.io.IOException => Array[Byte]()
    }
  }
    
  def getLikelyBytesForWad(bytes: Array[Byte]) = {
    // fully blocking reading of resource....good enough for now, because not a large number of simultaneous connections are expected
    // this code is ugly because it handles .wad and non zips simultaneously....TODO: make it prettier
    val zipInputStream = new ZipInputStream(new ByteArrayInputStream(bytes))
    var zipEntry = zipInputStream.getNextEntry
  
    if (zipEntry != null) {
      // we have a zip file, find the first entry with a .WAD extension
      var wadEntryFound = false
      while (zipEntry != null && !wadEntryFound)
      {
        if (zipEntry.getName.endsWith(".WAD") || zipEntry.getName.endsWith(".wad")) wadEntryFound = true
        else zipEntry = zipInputStream.getNextEntry
      }
      val (fileName, wadInputStream): (String, ZipInputStream) = 
        if (wadEntryFound) (zipEntry.getName, zipInputStream)
        else {
          // no entry found with the expected file extension, just try to parse the first file in the zip then
          val newStream = new ZipInputStream(new ByteArrayInputStream(bytes))
          val firstEntry = newStream.getNextEntry
          (firstEntry.getName, newStream)
        }
      (fileName, Stream.continually(wadInputStream.read()).takeWhile(_ != -1).map(_.toByte).toArray)
    }
    else ("", bytes)
  }
    
  case class NamedImage(name: String, image: ConvertPixelsToPNG.Image)
    
  def imageFromGraphic(palette: DoomWad.Palette)(graphic: DoomWad.Graphic) = {
    NamedImage(graphic.name, new ConvertPixelsToPNG.Image {
      val width = graphic.width
      val height = graphic.height
      def apply(x: Int, y: Int): Int = DoomAssets.pixelToColorInt(graphic.pixels(y)(x), palette)
    })
  }
  
  def imageFromFlat(palette: DoomWad.Palette)(flat: DoomWad.Flat) = {
    NamedImage(flat.name, new ConvertPixelsToPNG.Image {
      val width = 64
      val height = 64
      def apply(x: Int, y: Int): Int = DoomAssets.pixelToColorInt(flat.pixels(y)(x), palette)
    })
  }
  
  // TODO: merge pNames and patches, so pName index produces a Patch with only one level of indirection
  def imageFromTexture(pNames: Vector[DoomWad.PName], patches: List[DoomWad.Graphic], palette: DoomWad.Palette)(texture: DoomWad.Texture) = {
    case class ReferencedPatch(patch: DoomWad.Graphic, xOffset: Int, yOffset: Int)
    
    val referencedPatches: List[ReferencedPatch] = for {
      patchReference <- texture.patchReferences
      patch <- {
        val namesOfReferencedPatch = pNames(patchReference.indexIntoPNames).name.toLowerCase
        patches find(_.name.toLowerCase == namesOfReferencedPatch)  // some patch names don't match exactly by case
      }          
    } yield ReferencedPatch(patch, patchReference.xOffset, patchReference.yOffset)
    
    if (referencedPatches.isEmpty) None  // avoid rendering textures that will just be transparent
    // (some Wad seem to have bad textures like this in them (http://www.doomworld.com/idgames/levels/doom/Ports/v-z/vengnce.zip, Texture)
    // ...little cost to keeping these textures in the WAD (probably tens of bytes), so probably left over stuff form development)
    else {
      Some(NamedImage(texture.name, new ConvertPixelsToPNG.Image {
        val width = texture.width
        val height = texture.height
        def apply(x: Int, y: Int): Int = {
          // search all referenced patches to the 'top-most' pixel that's not transparent
          val finalPixel = referencedPatches.foldLeft(DoomWad.Transparent().asInstanceOf[DoomWad.Pixel])((pixel, referencedPatch) => {
            val ReferencedPatch(patch, xOffset, yOffset) = referencedPatch
            val xInPatch = x - xOffset
            val yInPatch = y - yOffset
            if ((xInPatch >= 0 && xInPatch < patch.width) && (yInPatch >= 0 && yInPatch < patch.height)) {
              val patchPixel = patch.pixels(yInPatch)(xInPatch)
              if (patchPixel == DoomWad.Transparent()) pixel else patchPixel
            }
            else pixel // let the current pixel pass through
          })
          DoomAssets.pixelToColorInt(finalPixel, palette)
        }
      }))
    }
  }
  
  def createJsonObjectForDoomWadImages(doomWad: DoomWad, fileName: String) = {
    def jsObjectFromNamedImage(namedImage: NamedImage): JsObject = {
      val NamedImage(name, image) = namedImage
      
      val pngBytes = ConvertPixelsToPNG(image)
      val pngInBase64 = java.util.Base64.getEncoder.encodeToString(pngBytes)
      
      JsObject(Seq(
          "name" -> JsString(name),
          "imageSrc" -> JsString("data:image/png;base64," + pngInBase64)))
    }
    
    val (doomAssets, differenceToShow) = doomWad match {
      case wad: DoomIWad => (new DoomAssets(wad), new DoomAssets.Difference(wad.textures, wad.flats, wad.sprites, wad.otherGraphics))
      case wad: DoomPWad => {
        val patchedAssets = standardDoomAssets applyPatch wad
        (patchedAssets, patchedAssets - standardDoomAssets)
      }
    }
    
    val flatProcessor = imageFromFlat(doomAssets.palettes.head) _
    val graphicsProcessor = imageFromGraphic(doomAssets.palettes.head) _
    val textureProcessor = imageFromTexture(doomAssets.pNames, doomAssets.patches, doomAssets.palettes.head) _
    
    val textures = JsArray(for {
        texture <- differenceToShow.textures
        image <- textureProcessor(texture)
      } yield jsObjectFromNamedImage(image))
    val sprites = JsArray(differenceToShow.sprites map {sprite => jsObjectFromNamedImage(graphicsProcessor(sprite))})
    val flats = JsArray(differenceToShow.flats map {flat => jsObjectFromNamedImage(flatProcessor(flat))})
    val otherGraphics = JsArray(differenceToShow.otherGraphics map {otherGraphic => jsObjectFromNamedImage(graphicsProcessor(otherGraphic))})
    
    JsObject(Seq(
        "fileName" -> JsString(fileName),
        "textures" -> textures,
        "sprites" -> sprites,
        "flats" -> flats,
        "otherGraphics" -> otherGraphics))
  }
  
  def extractDoomWadDataFromBinary = Action { implicit request => {
    
    val jsonResponse = extractDoomWadDataFromBytes((for {
        rawBuffer <- request.body.asRaw
        bytes <- rawBuffer.asBytes(10000000)  // ~10 Megs seems like a fine file size limit
      } yield bytes).getOrElse(Array[Byte]()))
            
    Ok(Json.stringify(jsonResponse)).as("application/json")
  }}
  
  def extractDoomWadDataFromUrl(url: String) = Action { implicit request => {
    // decode uri, using method mentioned here?: http://stackoverflow.com/questions/28385462/encoding-decoding-url-between-play-backend-and-javascript-client
    // but choosing UTF_8
    // NOT needed, Play has already decoded the URL by the time we've gotten here!  wow
    
    val jsonResponse = extractDoomWadDataFromBytes(getBytesFromURL(url))
            
    Ok(Json.stringify(jsonResponse)).as("application/json")
  }}
  
  def extractDoomWadDataFromBytes(getBytes: => Array[Byte]) = {
    // decode uri, using method mentioned here?: http://stackoverflow.com/questions/28385462/encoding-decoding-url-between-play-backend-and-javascript-client
    // but choosing UTF_8
    // NOT needed, Play has already decoded the URL by the time we've gotten here!  wow
    
    val timeAtStart = System.nanoTime()
    
    val (fileName, bytes) = getLikelyBytesForWad(getBytes)
    
    val timeAfterDownloadingFile = System.nanoTime()
    
    val parseResult = doomWadParser.parseDoomWad(new ByteReader(bytes))
    
    val timeAfterParsingFile = System.nanoTime()
    
    val result = parseResult match {
      case doomWadParser.Success(parsedWad, _) => createJsonObjectForDoomWadImages(parsedWad, fileName)
      case doomWadParser.Failure(msg, _) => JsString(msg)
      case doomWadParser.Error(msg, _) => JsString(msg)
    }
    
    /*
    val result = 
    JsObject(Seq(
        "fileName" -> JsString(""),
        "textures" -> JsArray(),
        "sprites" -> JsArray(),
        "flats" -> JsArray(),
        "otherGraphics" -> JsArray()))
     * 
     */
    
    val timeAfterBuildingImages = System.nanoTime()
    
    val timings = JsObject(Seq(
            "retrieveFile" -> JsNumber((timeAfterDownloadingFile - timeAtStart) / 1000000),
            "parseFile" -> JsNumber((timeAfterParsingFile - timeAfterDownloadingFile) / 1000000),
            "buildImages" -> JsNumber((timeAfterBuildingImages - timeAfterParsingFile) / 1000000)))
    
    JsObject(Seq(
      "timings" -> timings,
      "result" -> result,
      "totalMemory" -> JsNumber(sys.runtime.totalMemory),
      "freeMemory" -> JsNumber(sys.runtime.freeMemory),
      "maxMemory" -> JsNumber(sys.runtime.maxMemory)))
  }
}
