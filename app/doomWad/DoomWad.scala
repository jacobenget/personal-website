package doomWad

object DoomWad {
  sealed trait WADType
  case class IWAD() extends WADType
  case class PWAD() extends WADType
  
  case class Color(red: Int, green: Int, blue: Int)
  case class Palette(colors: Vector[Color])  // always has 256 colors
  
  case class PName(name: String)
  
  case class PatchReference(indexIntoPNames: Int, xOffset: Int, yOffset: Int)  // TODO replace x,y with something more descriptive
  case class Texture(name: String, width: Int, height: Int, patchReferences: List[PatchReference])
  
  sealed trait Pixel
  case class PaletteIndex(index: Int) extends Pixel
  case class Transparent() extends Pixel
  
  case class Graphic(name: String, width: Int, height: Int, val pixels: Vector[Vector[Pixel]], val leftOffset: Int, val topOffset: Int)
  
  case class Flat(name: String, val pixels: Vector[Vector[PaletteIndex]])  // always 64x64, and pixels are never transparent
}

class DoomWad (
    val palettes: List[DoomWad.Palette] = Nil,
    val textures: List[DoomWad.Texture] = Nil,
    val pNames: Vector[DoomWad.PName] = Vector.empty,  // Vector because we benefit from O(1) random access when interpreting textures
    val flats: List[DoomWad.Flat] = Nil,
    val patches: List[DoomWad.Graphic] = Nil,
    val sprites: List[DoomWad.Graphic] = Nil,
    val otherGraphics: List[DoomWad.Graphic] = Nil
){
  override def toString = {
    val palettesString = s"palettes: ${palettes.size}"
    val texturesString = s"textures: ${textures.size}"
    val pNamesString = s"pNames: ${pNames.size}"
    val flatsString = s"flats: ${flats.size}"
    val patchesString = s"patches: ${patches.size}"
    val spritesString = s"sprites: ${sprites.size}"
    val otherGraphicsString = s"otherGraphics: ${otherGraphics.size}"
    s"DoomWad: \n ${palettesString}\n ${texturesString}\n ${pNamesString}\n ${flatsString}\n ${patchesString}\n ${spritesString}\n ${otherGraphicsString}"
  }
}

class DoomIWad (
    override val palettes: List[DoomWad.Palette] = Nil,
    override val textures: List[DoomWad.Texture] = Nil,
    override val pNames: Vector[DoomWad.PName] = Vector.empty,  // Vector because we benefit from O(1) random access when interpreting textures
    override val flats: List[DoomWad.Flat] = Nil,
    override val patches: List[DoomWad.Graphic] = Nil,
    override val sprites: List[DoomWad.Graphic] = Nil,
    override val otherGraphics: List[DoomWad.Graphic] = Nil
) extends DoomWad

class DoomPWad (
    override val palettes: List[DoomWad.Palette] = Nil,
    override val textures: List[DoomWad.Texture] = Nil,
    override val pNames: Vector[DoomWad.PName] = Vector.empty,  // Vector because we benefit from O(1) random access when interpreting textures
    override val flats: List[DoomWad.Flat] = Nil,
    override val patches: List[DoomWad.Graphic] = Nil,
    override val sprites: List[DoomWad.Graphic] = Nil,
    override val otherGraphics: List[DoomWad.Graphic] = Nil
) extends DoomWad

object DoomAssets {
  def pixelToColorInt(pixel: DoomWad.Pixel, palette: DoomWad.Palette): Int = {
    pixel match {
      case DoomWad.PaletteIndex(index) => {
        val color = palette.colors(index)
        0xff000000 | ((color.blue & 0xff) << 16) | ((color.green & 0xff) << 8) | ((color.red) & 0xff)
      }
      case DoomWad.Transparent() => 0x00000000
    }
  }
  
  class Difference(
    val textures: List[DoomWad.Texture] = Nil,
    val flats: List[DoomWad.Flat] = Nil,
    val sprites: List[DoomWad.Graphic] = Nil,
    val otherGraphics: List[DoomWad.Graphic] = Nil
    )
}

// DoomAssets represents a full set of assets usable by Doom
// This could either be built from a DoomIWad, or from another DoomAssets that's been patched with an DoomPWad
class DoomAssets (
    val palettes: List[DoomWad.Palette],
    val textures: List[DoomWad.Texture],
    val pNames: Vector[DoomWad.PName],  // Vector because we benefit from O(1) random access when interpreting textures,
    val flats: List[DoomWad.Flat],
    val patches: List[DoomWad.Graphic],
    val sprites: List[DoomWad.Graphic],
    val otherGraphics: List[DoomWad.Graphic]
) {
  def this(doomIWad: DoomIWad) = this(
      doomIWad.palettes,
      doomIWad.textures,
      doomIWad.pNames,
      doomIWad.flats,
      doomIWad.patches,
      doomIWad.sprites,
      doomIWad.otherGraphics
      )
  
  def applyPatch(doomPWad: DoomPWad) = {
    // palettes, pNames, flats, and sprites are replaced wholesale by the patch, if they exist in the patch
    // (this is expected for palettes and pNames, because they are just a single lump,
    // but the fact this is the case for flats and spites is a limitation of Doom (even true of the almost-final Doom release 1.666 http://doomlegacy.sourceforge.net/hosted/doomspec1666.txt ))
    var palettes = if (doomPWad.palettes.isEmpty) this.palettes else doomPWad.palettes
    var pNames = if (doomPWad.pNames.isEmpty) this.pNames else doomPWad.pNames
    var flats = if (doomPWad.flats.isEmpty) this.flats else doomPWad.flats
    var sprites = if (doomPWad.sprites.isEmpty) this.sprites else doomPWad.sprites

    // textures, patches, otherGraphics are replaced on a per name basis
    var textures = {
      val inheritedTextures = (this.textures filter {baseTexture => doomPWad.textures.forall {patchTexture => patchTexture.name != baseTexture.name}})
      val newTextures = doomPWad.textures
      inheritedTextures ++ newTextures
    }
    var patches = {
      val inheritedPatches = (this.patches filter {basePatch => doomPWad.patches.forall {patchPatch => patchPatch.name != basePatch.name}})
      val newPatches = doomPWad.patches
      inheritedPatches ++ newPatches
    }
    var otherGraphics = {
      val inheritedOtherGraphics = (this.otherGraphics filter {baseOtherGraphic => doomPWad.otherGraphics.forall {patchOtherGraphic => patchOtherGraphic.name != baseOtherGraphic.name}})
      val newOtherGraphics = doomPWad.otherGraphics
      inheritedOtherGraphics ++ newOtherGraphics
    }
    
    // search otherGraphics, and any that are found that have their name listed in pNames, move them to patches
    val (areReallyPatches, leftOverOtherGraphics) = otherGraphics partition {
      otherGraphic => {pNames.exists {_.name.toLowerCase == otherGraphic.name.toLowerCase}}
    }
    
    patches = areReallyPatches ++ patches
    otherGraphics = leftOverOtherGraphics
    
    new DoomAssets(palettes, textures, pNames, flats, patches, sprites, otherGraphics)
  }
  
  def -(that: DoomAssets): DoomAssets.Difference = {
    // flats and sprite are always replaced wholesale, so we can just check pointer equality
    val flatDifference = if (this.flats eq that.flats) Nil else this.flats
    val spriteDifference = if (this.sprites eq that.sprites) Nil else this.sprites
    
    // otherGraphics are different only if they don't appear in the graphics of 'that'
    val otherGraphicsDifference = this.otherGraphics filter {thisOtherGraphic => that.otherGraphics forall {!_.eq(thisOtherGraphic)}}
  
    // textures are different if they don't appear in the textures of 'that'
    val (sameTextures, completelyNewTextures) = this.textures partition {thisTexture => that.textures exists {_.eq(thisTexture)}}
    // OR if they point to a different pName
    val (textureWithChangedReferencedPName, sameTextureWithSamePNames) = {
      if (this.pNames eq that.pNames) (Nil, sameTextures)
      else sameTextures partition {_.patchReferences exists {patchRef => this.pNames(patchRef.indexIntoPNames).name.toLowerCase != that.pNames(patchRef.indexIntoPNames).name.toLowerCase}}
    }
    // OR if the pName they point to references a patch that's changed
    val textureWithChangedPatch = {
      val patchDifference = this.patches filter {thisPatch => that.patches forall {!_.eq(thisPatch)}}
      sameTextureWithSamePNames filter {_.patchReferences exists {patchRef => patchDifference exists {_.name.toLowerCase == this.pNames(patchRef.indexIntoPNames).name.toLowerCase}}}
    }
    
    new DoomAssets.Difference(
        completelyNewTextures ::: textureWithChangedReferencedPName ::: textureWithChangedPatch,
        flatDifference,
        spriteDifference,
        otherGraphicsDifference)
  }
}