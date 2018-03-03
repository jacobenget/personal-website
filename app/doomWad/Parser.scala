
package doomWad
import DoomWad._

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input.{ Position, Reader, Positional }
import scala.util.parsing.input.CharArrayReader.EofCh

// utility to truncate a Reader[T] to a new length
class ReaderTruncated[T](sourceReader: Reader[T], length: Int) extends Reader[T] {
  // implementation of Reader[T]
  override def atEnd: Boolean = sourceReader.atEnd | (length == 0)
  override def first: T = sourceReader.first
  override def pos: Position = sourceReader.pos
  override def rest: Reader[T] = if (atEnd) this else new ReaderTruncated(sourceReader.rest, length - 1)
}

class ByteReader(val bytes: Array[Byte], override val offset: Int) extends Reader[Byte] {
  assert(offset >= 0)
  def this(bytes: Seq[Byte]) = this(bytes.toArray, 0)

  // implementation of Reader[Byte]
  override def atEnd: Boolean = offset >= bytes.length
  override def first: Byte = if (atEnd) EofCh.toByte else bytes(offset) 
  override def pos: Position = new Position{
    override final val line = 1
    override def column = offset + 1
    override def lineContents: String = "" /* could not find documentation on this abstract field, and yet it is required when implementing Position */}
  override def rest: ByteReader = if (atEnd) this else new ByteReader(bytes, offset + 1)
  
  // can have an O(1) implementation of 'drop' (instead of the default, which can only be O(n) because it just uses Reader[T] interface), so override this method
  override def drop(n: Int): ByteReader = {
    new ByteReader(bytes, offset + n)
  }
  
  // might as well implement this, since we can support converting the Bytes to Char
  override def source = bytes drop(offset) map (_.toChar)
}

class DoomWadParser extends Parsers {
  type Elem = Byte
  
  // assumes the bytes being read represent integers values in Little Endian!
  // and that the data uses 2's complement to represent negative values! (because it assumes the representation is the same as Java's, which is 2's complement))
  def byteListToInt(bytes: List[Int]): Int = bytes.foldRight(0)((byte, sum) => (sum << 8) + (byte & 0xFF))

  val uint8: Parser[Int] = elem("anyElem", _ => true) ^^ {_ & 0xff}  // returning Int instead of Byte to have the value range be [0, 255] instead of [-128, 127], because we need unsigned values
  val int16: Parser[Short] = uint8~uint8 ^^ { case firstByte~secondByte => {
    val bb = java.nio.ByteBuffer.allocate(2);
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
    bb.put(firstByte toByte);
    bb.put(secondByte toByte);
    bb.getShort(0);
  }}
  val uint16: Parser[Int] = repN(2, uint8) ^^ byteListToInt
  val int32: Parser[Int] = repN(4, uint8) ^^ byteListToInt
  val string8: Parser[String] = repN(8, uint8) ^^ ( byteList => new String(byteList.takeWhile(_ != 0).map(_.toChar).toArray) )
  
  // parser for skipping the next 'num' bytes
  def skip(num: Int) = new Parser[Unit] {
    def apply(in: Input) = {
      if (num == 0) Success((), in)
      else {
        val lastToSkip = in.drop(num - 1)
        if (lastToSkip.atEnd) Failure("Cannot skip the required number of bytes: " + num, in)
        else Success((), lastToSkip.drop(1))
      }
    }
  }
  
  case class Marker() extends Positional
  
  // parser to return the current position of the input stream
  def getPos: Parser[Position] = positioned(success(new Positional{})) ^^ {positional => positional.pos}
  
  implicit def stringLiteral(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      if (s exists(c => c.toByte.toChar != c)) Failure("parser has a string literal with non ASCII characters!", in)  // TODO: this decision can be made on parser construction, before parsing starts
      else {
        // this approach uses local state, wonder if there's a better way to implement this
        var currentInput = in
        if (s forall(c => {
          if (currentInput.atEnd || (c.toByte != currentInput.first)) false
          else { currentInput = currentInput.rest; true}
          })) Success(s, currentInput)
        else  Failure("did not match string literal", currentInput)  // TODO: keep track of where the failure happened in the string literal
      }
    }
  }
 
  implicit def regexLiteral(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      r.findFirstIn(in.source) match {  // TODO! don't convert the entire byte stream into a String just to do a regex match, instead implement CharSequence manually
        case None => Failure("did not match regex '" + r + "'", in)
        case Some(s) => Success(s, in.drop(s.length))
      }
    }
  }
 
  implicit def byteLiteral(b: Byte): Parser[Byte] = new Parser[Byte] {
    def apply(in: Input) = {
      if (in.atEnd) Failure("unexpected end of input searching for byte '" + b + "'", in)
      else if (in.first == b) Success(b, in.drop(1))
      else Failure("Found byte '" + in.first + "' where byte '" + b + "' was expected", in)
    }
  }
  
  case class DirectoryEntry(name: String, offset: Int, length: Int)
  
  
  sealed trait Lump
  
  case class PlayPalLump(palettes: List[Palette]) extends Lump
  
  case class PNamesLump(names: List[String]) extends Lump
  
  case class TexturesLump(textureLumpName: String, textures: List[Texture]) extends Lump
  
  case class FlatLump(flat: Flat) extends Lump
  
  sealed trait GraphicsType
  case class Sprite() extends GraphicsType
  case class Patch() extends GraphicsType
  case class Other() extends GraphicsType
  case class GraphicLump(gType: GraphicsType, graphic: Graphic) extends Lump
  
  
  // BNF-like expressions for lump sections retrieved
  // from The Unofficial Doom Specs: http://doomlegacy.sourceforge.net/hosted/doomspec1666.txt
  
  
    /*
      <PLAYPAL>       := <palette> [14]
      <palette>       := {<red> <green> <blue>} [256]
      <red>           := <byte>
      <green>         := <byte>
      <blue>          := <byte>
    */
  
  def playPal: Parser[PlayPalLump] = repN(14, repN(256, uint8~uint8~uint8)) ^^ (palettes =>
        PlayPalLump(palettes map {palette =>
          Palette(palette map {case red~green~blue => Color(red, green, blue)} toVector)}))
        
          
    /*
      <PNAMES>        :=  <num_pnames>         ;<long>
                          <pnames>
      <pnames>        :=  <pname> [num_pnames]
      <pname>         :=  <string8>           ;match the <name> from the
                                              ;<lumpinfo> of a <picture>
    */
          
  def pNames: Parser[PNamesLump] = for {
    nameCount <- int32
    names <- repN(nameCount, string8)
  } yield PNamesLump(names)
  
  
    /*
      <TEXTURE1>      :=  <num_textures>       ;<long>
                          <tex_offsets>
                           <tex_entries>
      <tex_offsets>   :=  <tex_offset> [num_textures]
      <tex_offset>    :=  <long>
      <tex_entries>   :=  <tex_entry> [num_textures]
      <tex_entry>     :=  <tex_name>           ;<string8>
                          <short:0>
                          <short:0>
                          <tex_width>          ;<short>
                          <tex_height>         ;<short>
                          <short:0>
                          <short:0>
                          <num_patches>        ;<short>
                          <patches>
      <patches>       :=  <patch> [num_patches]
      <patch>         :=  <x_offset>           ;all are <short>
                          <y_offset>
                          <pname_number>       ;lookup in <PNAMES> for picture
                          <short:1>            ;supposedly <stepdir>
                          <short:0>            ;supposedly <color_map>
    */
  
  def textures(textureLumpName: String): Parser[TexturesLump] = for {
    textureOffsets <- guard(int32 >> {textureCount => repN(textureCount, int32)})  // texture offsets are relative to the beginning of the texture lump, so we must guard to ensure the reader can restart from there
    textureList <- {
      val initialParser = success(List[Texture]())
      
      val continueParserOnNextTexture = (currentParser: Parser[List[Texture]], nextOffset: Int) => for {
        texturesSoFar <- guard(currentParser)  // texture offsets are relative to the beginning of the texture lump, so we must guard to ensure the reader can restart from there
        _ <- skip(nextOffset)
        name~_~width~height~_~patchCount <- string8~skip(4)~int16~int16~skip(4)~int16
        patchReferences <- repN(patchCount, int16~int16~int16<~skip(4)) ^^ {
          _ map {case xOffset~yOffset~indexIntoPNames => PatchReference(indexIntoPNames, xOffset, yOffset)}
        }
      } yield Texture(name, width, height, patchReferences)::texturesSoFar
        
      textureOffsets.foldLeft(initialParser)(continueParserOnNextTexture)
    }
  } yield TexturesLump(textureLumpName, textureList)
  
  
    /*
      <flat>          := <colorbyte> [64*64]   ;<byte>   // in row major order!
    */
  
  def flat(name: String): Parser[FlatLump] = repN(64, repN(64, uint8)) ^^ {pixelGrid => {
    val convertedPixels = pixelGrid map {_ map {pixel => PaletteIndex(pixel & 0xff /*to turn Byte into Int while ignoring sign*/)} toVector} toVector
    val doomWadFlat = Flat(name, convertedPixels)  // without this temp value, a compiler error occurs :(
    FlatLump(doomWadFlat)
  }}
  
  
    /*
      <picture>       :=  <header>
                          <pointers>           ;offsets to <column> starts
                          <pixel_data>
      <header>        :=  <width>              ;all are <short>
                          <height>
                          <left_offset>
                          <top_offset>
      <pointers>      :=  <pointer> [width]    ;<long>
      <pixel_data>    :=  <column> [width]
      <column>        :=  <post> [...]
                          <byte:255>           ;255 (0xff) ends the column
      <post>          :=  <rowstart>           ;<byte>
                          <num_pixels>         ;<byte>
                          <unused>             ;<byte>
                          <pixels>
                          <unused>             ;<byte>
      <pixels>        :=  <pixel> [num_pixels] ;<byte>
    */
  
  /*
  def graphic(name: String, graphicType: GraphicsType): Parser[GraphicLump] = for {
    (width, height, leftOffset, topOffset, columnOffsets) <- {
      val header = int16~int16~int16~int16
      
      val headerAndColumnOffsets = for {
        width~height~leftOffset~topOffset <- header >> {head => head match {
          case width~height~left~top =>{
            if (width <= 0 || height <= 0) failure(s"non valid graphic size: (${width} X ${height})")
            else success(head)}}
          }
        pointers <- repN(width, int32)
      } yield (width, height, leftOffset, topOffset, pointers)
      
      guard(headerAndColumnOffsets)  // guard used because column offsets are relative to the beginning of the lump,
        // so when we interpret them next we need to have the reader reset to the start of the lump
    }
    pixels <- {
      // read all columns, and use the column data to edit the graphic
      
      // great an empty graphic, all pixels are transparent by default
      val defaultPixel: Pixel = Transparent()
      val graphic = Array.fill(height, width)(defaultPixel)
      
      // read a single column, using its column data to edit the graphic
      val continueParserOnNextColumn = (currentParser: Parser[Array[Array[Pixel]]], nextOffsetWithIndex: (Int, Int)) => {
        val (columnOffset, columnIndex) = nextOffsetWithIndex
        if (columnOffset < 0) failure("column offset for column of patch data is negative!")
        else {
          for {
            graphicSoFar <- guard(currentParser)  // texture offsets are relative to the beginning of the texture lump, so we must guard to ensure the reader can restart from there
            _ <- skip(columnOffset)
            _ <- {
              val columnSection: Parser[Int~List[Int]] = (uint8~uint8<~skip(1)) >> {
                case rowStart~pixelCount => if (pixelCount < 0) failure("negative number of pixels in patch column section!") else success(rowStart)~repN(pixelCount, uint8)
              }<~skip(1)
              val columnEnd = 255.toByte
              rep(not(columnEnd)~>columnSection)<~columnEnd >> {columnSection => {
                var indexOutOfBounds = false
                for {
                  rowStart~pixels <- columnSection
                  (pixel, index) <- pixels.zipWithIndex
                } yield {
                  val rowIndex = rowStart + index
                  val rowIndexOutOfBounds = rowIndex < 0 || rowIndex >= graphicSoFar.length
                  if (rowIndexOutOfBounds)
                    indexOutOfBounds = true
                  else if (columnIndex < 0 || columnIndex >= graphicSoFar(rowIndex).length)
                    indexOutOfBounds = true
                  else
                    graphicSoFar(rowStart + index)(columnIndex) = PaletteIndex(pixel & 0xff)
                }
                if (indexOutOfBounds) failure("Array index for row data is out of bounds!")
                else success(())
              }}
            }
          } yield graphicSoFar
        }
      }
        
      columnOffsets.zipWithIndex.foldLeft(success(graphic))(continueParserOnNextColumn) ^^ {_ map {_.toVector} toVector}
    }
  } yield GraphicLump(graphicType, Graphic(name, width, height, pixels, leftOffset, topOffset))
  */
  
  def graphic(name: String, graphicType: GraphicsType): Parser[GraphicLump] = for {
    (width, height, leftOffset, topOffset, columnOffsets) <- {
      val header = int16~int16~int16~int16
      
      val headerAndColumnOffsets = for {
        width~height~leftOffset~topOffset <- header >> {head => head match {
          case width~height~left~top =>{
            if (width <= 0 || height <= 0) failure(s"non valid graphic size: (${width} X ${height})")
            else success(head)}
          }
        }
        pointers <- repN(width, int32)
      } yield (width, height, leftOffset, topOffset, pointers)
      
      guard(headerAndColumnOffsets)  // guard used because column offsets are relative to the beginning of the lump,
        // so when we interpret them next we need to have the reader reset to the start of the lump
    }
    pixels <- {
      // read all columns, and in the process produce a Map that relays the pixel information per coordinate
      
      // read a single column, using its column data to edit the Map
      val continueParserOnNextColumn = (currentParser: Parser[Map[(Int, Int), Pixel]], nextOffsetWithIndex: (Int, Int)) => {
        val (columnOffset, columnIndex) = nextOffsetWithIndex
        if (columnOffset < 0) failure("column offset for column of patch data is negative!")
        else {
          for {
            pixelMapSoFar <- guard(currentParser)  // texture offsets are relative to the beginning of the texture lump, so we must guard to ensure the reader can restart from there
            _ <- skip(columnOffset)
            newPixelMap <- {
              val columnSection: Parser[Int~List[Int]] = (uint8~uint8<~skip(1)) >> {
                  case rowStart~pixelCount => if (pixelCount < 0) failure("negative number of pixels in patch column section!") else success(rowStart)~repN(pixelCount, uint8)
                }<~skip(1)
              val columnEnd = 255.toByte
              rep(not(columnEnd)~>columnSection)<~columnEnd ^^ {columnSection => pixelMapSoFar ++ {
                for {
                  rowStart~pixels <- columnSection
                  (pixel, index) <- pixels.zipWithIndex
                } yield ((columnIndex, rowStart + index) -> PaletteIndex(pixel & 0xff))
              }}
            }
          } yield newPixelMap
        }
      }
      
      columnOffsets.zipWithIndex.foldLeft(success(Map[(Int, Int), Pixel]()))(continueParserOnNextColumn) >> { pixelMap => {
        // verify that all pixel are within the valid bounds of the graphic
        if (! pixelMap.forall{ case ((x, y), _) => (x >= 0) && (x < width) && (y >= 0) && (y < height)})
          failure("graphic has an invalid pixel coord")
        else {
          // great an empty graphic, all pixels are transparent by default
          val defaultPixel: Pixel = Transparent()
          var graphic = Array.fill(height, width)(defaultPixel)
          
          // and then fill that graphic with pixel data
          for {
            ((x, y), pixel) <- pixelMap
          } graphic(y)(x) = pixel
          
          // TODO! just return the Map, no reason to write it out to an Array
          success(graphic map {_.toVector} toVector)
        }
      }}
    }
  } yield GraphicLump(graphicType, Graphic(name, width, height, pixels, leftOffset, topOffset))
  
  
  def parsersForDirectoryEntries(entries: List[DirectoryEntry]) = {
    var entriesLeft = entries
    
    val playPalParsers = entriesLeft partition (_.name == "PLAYPAL") match {
      case (found, others) => {
        entriesLeft = others
        found map ((_, playPal))
      }
    }
    
    val pNameParsers = entriesLeft partition (_.name == "PNAMES") match {
      case (found, others) => {
        entriesLeft = others
        found map ((_, pNames))
      }
    }
    
    val textureParsers = {
      val textureEntryName = """TEXTURE[12]""".r
      entriesLeft partition (_.name match { case textureEntryName(_*) => true; case _ => false}) match {
        case (found, others) => {
          entriesLeft = others
          found map (entry => (entry, textures(entry.name)))
        }
      }
    }
    
    //************* continue here, like above, recognizing other graphics, because it seems that pnames can reference those!!
    
    val flatParsers = {
      val flatMarkerName = """F[12]_(START|END)""".r
      entriesLeft span(_.name != "F_START") match {
        case (before, fStart::flatsAndRest) => {
          flatsAndRest span(_.name != "F_END") match {
            case (flats, fEnd::after) => {
              entriesLeft = before ::: after
              val flatsWithMarkersRemoved = flats filter (_.name match { case flatMarkerName(_*) => false; case _ => true})
              flatsWithMarkersRemoved map (entry => (entry, flat(entry.name)))
            }
            case _ => Nil  // no "F_END" marker!
          }
        }
        case (before, Nil) => Nil
      }
    }
    
    val patchParsers = {
      val patchMarkerName = """P[12]_(START|END)""".r
      entriesLeft span(_.name != "P_START") match {
        case (before, pStart::patchesAndRest) => {
          patchesAndRest span(_.name != "P_END") match {
            case (patches, pEnd::after) => {
              entriesLeft = before ::: after
              val patchesWithMarkersRemoved = patches filter (_.name match { case patchMarkerName(_*) => false; case _ => true})
              patchesWithMarkersRemoved map (entry => (entry, graphic(entry.name, Patch())))
            }
            case _ => Nil  // no "P_END" marker!
          }
        }
        case (before, Nil) => Nil
      }
    }
    
    val spriteParsers = {
      entriesLeft span(_.name != "S_START") match {
        case (before, sStart::spritesAndRest) => {
          spritesAndRest span(_.name != "S_END") match {
            case (sprites, sEnd::after) => {
              entriesLeft = before ::: after
              sprites map (entry => (entry, graphic(entry.name, Sprite())))
            }
            case _ => Nil  // no "P_END" marker!
          }
        }
        case (before, Nil) => Nil
      }
    }
    
    /*
    for {
      DirectoryEntry(name, _, _) <- entriesLeft
    } println(s"still remaining: ${name}")
    println(s"total count: ${entriesLeft.length}")
    */
    /*
    def applyParserForLump(entryWithParser: (DirectoryEntry, Parser[Lump])) = new Parser[Lump] {
      def apply(in: Input) = {
        val parser = {
          val (entry, parser) = entryWithParser
          skip(entry.offset) ~> getPos~parser~getPos >> {case startPos~parsedLump~endPos => {
            if (startPos.line != endPos.line) failure(s"Lump parser changed lines unexpectedly from ${startPos.line} to ${endPos.line}")
            else {
              val amountRead = endPos.column - startPos.column
              val bytesNotRead = entry.length - amountRead
              if ((bytesNotRead < 0) || (bytesNotRead >= 4)) failure(s"Lump parser consumed ${endPos.column - startPos.column} bytes, when it should have consumed between ${entry.length - 3} and ${entry.length} bytes")
              else success(parsedLump)
            }
          }}
        }
        parser(new ReaderTruncated(in, entryWithParser._1.offset + entryWithParser._1.length))  // truncated length is offset+length because 'in' starts at the beginning of the file
      }
    }
     *
     */
    
    def applyParserForLump(entryWithParser: (DirectoryEntry, Parser[Lump])) = {
      val (entry, parser) = entryWithParser
      skip(entry.offset) ~> getPos~parser~getPos >> {case startPos~parsedLump~endPos => {
        if (startPos.line != endPos.line) failure(s"Lump parser changed lines unexpectedly from ${startPos.line} to ${endPos.line}")
        else {
          val amountRead = endPos.column - startPos.column
          val bytesNotRead = entry.length - amountRead
          if ((bytesNotRead < 0) || (bytesNotRead >= 4)) failure(s"Lump parser consumed ${endPos.column - startPos.column} bytes, when it should have consumed between ${entry.length - 3} and ${entry.length} bytes")
          else success(parsedLump)
        }
      }}
    }
    
    val knownLumps = (playPalParsers ::: pNameParsers ::: textureParsers ::: flatParsers ::: patchParsers ::: spriteParsers) map applyParserForLump
    //val knownLumps = (playPalParsers ::: flatParsers.take(1)) map applyParserForLump
    
    // some WAD files have non-named directory entries, no idea what the goal is behind that, but they cause a crash when reading as a graphic
    
    
    val otherGraphicsParsers = entriesLeft map (entry => (entry, graphic(entry.name, Other()))) map applyParserForLump map {opt(_)}
      
    val knownAsOption = knownLumps map {lumpParser => lumpParser >> {lump => success(Option(lump))}}
    
    knownAsOption ::: otherGraphicsParsers
  }
  
  def parseDirectory = for {
    wadType~dirLength~dirOffset <- guard((("PWAD" ^^ {_ => PWAD()}) | ("IWAD" ^^ {_ => IWAD()})) ~ int32 ~ int32)
    directoryList <- skip(dirOffset) ~> repN(dirLength, int32 ~ int32 ~ string8) ^^
      { _ map{ case offset~length~name => DirectoryEntry(name, offset, length) } }
  } yield (directoryList, wadType)
  
  def parseLumps = for {
    (directoryList, wadType) <- guard(parseDirectory)
    lumps <- {
      val initialParser = success(List[Option[Lump]]())
      val continueParserOnNextLump = (currentParser: Parser[List[Option[Lump]]], parserForThisLump: Parser[Option[Lump]]) => for {
        lumps <- guard(currentParser)  // run the parser so far, but guard it so that marker ends at the start of the WAD
        lump <- parserForThisLump  // run the next lump parser from the start of the WAD
      } yield lump::lumps
      parsersForDirectoryEntries(directoryList).foldLeft(initialParser)(continueParserOnNextLump)
    }
  } yield (wadType, lumps)
  
  def parseDoomWad = parseLumps >> { _ match { case (wadType, lumps) => {
    // create some mutable variables to accumulate the data as the lumps are encountered
    var palettes: List[DoomWad.Palette] = Nil
    var textures: List[DoomWad.Texture] = Nil
    var pNames: List[DoomWad.PName] = Nil
    var flats: List[DoomWad.Flat] = Nil
    var patches: List[DoomWad.Graphic] = Nil
    var sprites: List[DoomWad.Graphic] = Nil
    var otherGraphics: List[DoomWad.Graphic] = Nil
    
    // parsing should still fail in a few cases
    var foundMultiplePlayPalLumps = false
    var foundMultiplePNamesLumps = false
    
    for (Some(lump) <- lumps) lump match {
      case PlayPalLump(palettesInLump) => {foundMultiplePlayPalLumps = !(palettes isEmpty); palettes = palettesInLump}
      case TexturesLump(_, texturesInLump) => textures = texturesInLump ::: textures 
      case PNamesLump(names) => pNames = {foundMultiplePNamesLumps = !(pNames isEmpty); (names map {PName(_)})}
      case FlatLump(flat) => flats = flat :: flats
      case GraphicLump(Patch(), graphic) => patches = graphic :: patches
      case GraphicLump(Sprite(), graphic) => sprites = graphic :: sprites
      case GraphicLump(Other(), graphic) => otherGraphics = graphic :: otherGraphics
    }
    
    if (foundMultiplePlayPalLumps) failure("WAD file had multiple \"PLAYPAL\" lumps, where only one is allowed")
    else if (foundMultiplePNamesLumps) failure("WAD file had multiple \"PNAMES\" lumps, where only one is allowed")
    else wadType match {
      case IWAD() => success(new DoomIWad(palettes, textures, pNames toVector, flats, patches, sprites, otherGraphics))
      case PWAD() => success(new DoomPWad(palettes, textures, pNames toVector, flats, patches, sprites, otherGraphics))
    }
  }}}
}