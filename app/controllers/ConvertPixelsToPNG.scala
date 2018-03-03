package controllers

import javax.imageio.{ImageWriteParam, IIOImage, ImageWriter, ImageIO}
import collection.JavaConverters._
import java.awt.color.ColorSpace
import java.awt.image._
import java.awt.Transparency
import java.io._

object ConvertPixelsToPNG {
  private object pureimage {
    
    /*
     * Code copied from https://github.com/stephenjudkins/pureimage and then edited further:
  
     * MIT License
  
     * Copyright (C) 2013 Stephen Judkins
  
     * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
  
     * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
  
     * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
     */
    
    private implicit class AnyOps[A](val a: A) extends AnyVal {
        def |>[B](f: A => B):B = f(a)
        def tap[B](f: A => B) = {
          f(a)
          a
        }
      }
    
    private class StubSampleModel(w: Int, h: Int, colors: Int, val colorModel: ComponentColorModel) extends SampleModel(DataBuffer.TYPE_INT, w, h, colorModel.getNumComponents()) {
    
      private def ??? = throw new Exception("Not implemented")
    
    
      def getSampleSize = (1 to colorModel.getNumComponents).map(_ => 8).toArray
      def getSampleSize(i:Int) = 8
    
      def createDataBuffer() = ???
      def createSubsetSampleModel(a: Array[Int]) = ??? //this//originalSampleModel.createSubsetSampleModel(a)
      def createCompatibleSampleModel(x: Int, y: Int) = colorModel.createCompatibleSampleModel(x, y)//this //??? //createCompatibleSampleModel()//??? //this //originalSampleModel.createCompatibleSampleModel(x, y)
      def setSample(x$1: Int, x$2: Int, x$3: Int, x$4: Int, x$5: java.awt.image.DataBuffer) { ??? }
      def setDataElements(x$1: Int, x$2: Int, x$3: Any, x$4: java.awt.image.DataBuffer) { ??? }
      def getNumDataElements = colorModel.getNumComponents//originalSampleModel.getNumDataElements
      def getDataElements(x: Int, y: Int, out: Any, data: java.awt.image.DataBuffer) = ???
      def getSample(x: Int, y: Int, b: Int, dataBuffer: java.awt.image.DataBuffer) = ???
    }
    
    private case class OutputRaster(
       ourSampleModel: SampleModel,
       w: Int,
       h: Int,
       x: Int,
       y: Int,
       rgba: (Int, Int) => Int,
       perPixel: Int,
       offsetX: Int = 0,
       offsetY: Int = 0
    ) extends WritableRaster(ourSampleModel, new DataBufferInt(1), new java.awt.Point(0,0)) {
      this.numBands = perPixel
    
      override def getPixels(x0: Int, y0: Int, w0: Int, h0: Int, outArray: Array[Int]) = {
        val out = if (outArray == null) {
          new Array[Int](w0 * h0 * perPixel)
        } else {
          outArray
        }
    
        var i = 0
        while (i < w0) {
          var j = 0
          while (j < h0) {
            val x1 = i + x + offsetX
            val y1 = j + y + offsetY
            val idx = (j * w0 + i) * perPixel
    
            val p = rgba(x1, y1)
    
            for (band <- 0 to ourSampleModel.getNumBands()-1) {
              out(idx+band) = (p >> 8*band) & 0xff
            }
    
            j += 1
          }
          i += 1
        }
    
    
        out
      }
    
      override def createChild(parentX: Int, parentY: Int, w: Int, h: Int, minX: Int, minY: Int, bandList: Array[Int]) =
        copy(offsetX = offsetX + parentX, offsetY = offsetY + parentY, w = w, h = h)
    }
    
    def outputRGBAtoPNG(width: Int, height:Int, func: (Int, Int)=>Int): Array[Byte] = {
      val writer = ImageIO.getImageWritersByFormatName("png").asScala.toSeq.head
  
      /*
       * About ComponentColorModel constructor, from http://docs.oracle.com/javase/7/docs/api/java/awt/image/ComponentColorModel.html#ComponentColorModel(java.awt.color.ColorSpace,%20boolean,%20boolean,%20int,%20int)
       * 
       * public ComponentColorModel(ColorSpace colorSpace,
                     boolean hasAlpha,
                     boolean isAlphaPremultiplied,
                     int transparency,
                     int transferType)
       * 
       * Constructs a ComponentColorModel from the specified parameters. Color components will be in the specified ColorSpace.
       * The supported transfer types are DataBuffer.TYPE_BYTE, DataBuffer.TYPE_USHORT, DataBuffer.TYPE_INT, DataBuffer.TYPE_SHORT, DataBuffer.TYPE_FLOAT, and DataBuffer.TYPE_DOUBLE.
       * The number of significant bits per color and alpha component will be 8, 16, 32, 16, 32, or 64, respectively. 
       * The number of color components will be the number of components in the ColorSpace.
       * There will be an alpha component if hasAlpha is true.
       * If hasAlpha is true, then the boolean isAlphaPremultiplied specifies how to interpret color and alpha samples in pixel values.
       * If the boolean is true, color samples are assumed to have been multiplied by the alpha sample.
       * The transparency specifies what alpha values can be represented by this color model.
       * The acceptable transparency values are OPAQUE, BITMASK or TRANSLUCENT. The transferType is the type of primitive array used to represent pixel values.
       * */
      
      val colorModel = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB), true, false, Transparency.BITMASK, DataBuffer.TYPE_INT) {
        override def isCompatibleRaster(r: Raster) = true
      }
  
      val colors = colorModel.getNumComponents
  
      val sampleModel = new StubSampleModel(width, height, colors, colorModel)
  
      val raster = new OutputRaster(sampleModel, width, height, 0, 0, func, colors)
  
      val buffered = new BufferedImage(
        sampleModel.colorModel,
        raster,
        false,
        new java.util.Hashtable()
      ) {
        override def getData(rect: java.awt.Rectangle) = {
          raster.copy(x = rect.getX.toInt, y = rect.getY.toInt, w = rect.getWidth.toInt, h = rect.getHeight.toInt)
        }
      }
  
      (new ByteArrayOutputStream).tap({ o =>
        // type-safety FTW. :-/
        writer.setOutput(ImageIO.createImageOutputStream(o))
        val iioImage = new IIOImage(buffered, null, null)
        val param = writer.getDefaultWriteParam
        writer.write(null, iioImage, param)
      }).toByteArray
    }
  }
  
  trait Image {
    def width: Int
    def height: Int
    def apply(x: Int, y: Int): Int // output is an Int in rgba format, flipped, meaning bits are 0xAABBGGRR
  }
  
  // ASSUMES that pixels have alpha, but that the alpha is just a mask (i.e. alpha value is either 0x00 or 0xFF)
  def apply(image: Image): Array[Byte] = pureimage.outputRGBAtoPNG(image.width, image.height, (x: Int, y: Int) => image(x, y))
}