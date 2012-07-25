package com.fckawe.util.grafix
import com.fckawe.util.grafix.BitmapRangeMode._

/** TODO: scaladoc
  */
class Bitmap(width: Int, height: Int) {
    
    protected var pixels: Array[Int] = new Array[Int] (width * height)
    var rangeMode: BitmapRangeMode = BitmapRangeMode.NONE
    var rangeTL: (Int, Int) = _
    var rangeBR: (Int, Int) = _
    
    def getWidth: Int = width
    def getHeight: Int = height
    
    def getPixels: Array[Int] = pixels
    def setPixels(pixels: Array[Int]) = {this.pixels = pixels}
    
    /** Define a range which has to be excluded or exclusively included
      * (depending on the rangeMode) while processing the bitmap data.
      * @param rangeMode The BitmapRangeMode to use.
      * @param ul The coordinates of the upper left point of the range.
      * @param lr The coordinates of the lower right point of the range.
      */
    def setRange(rangeMode: BitmapRangeMode, tl: (Int, Int), br: (Int, Int)) = {
        this.rangeMode = rangeMode
        this.rangeTL = tl
        this.rangeBR = br
    }
    
    /** Reset a previously defined range.
      */
    def resetRange() {
        rangeMode = BitmapRangeMode.NONE
        rangeTL = (0, 0)
        rangeBR = (0, 0)
    }
    
    /** Clears the bitmap (or the previously defined range) with the given
      * color code.
      * @param color The RGB index of the color the clear with.
      */
    def clear(color: Int) {
        if(rangeMode == null || rangeMode == BitmapRangeMode.NONE) {
            // fill the whole bitmap with the given color
            pixels = Array.fill(pixels.length) {color}
        } else if(rangeMode == BitmapRangeMode.ONLY) {
            // only fill the given range
            (rangeTL._2 until rangeBR._2).foreach(y => {
                val lineStartOffset = (y * width) + rangeTL._1
                (0 until width).foreach(x => pixels(lineStartOffset + x) = color)
            })
        }
    }
    
    /** TODO: scaladoc
      */
    def blit(bitmap: Bitmap, posX: Int, posY: Int) {
        blitInternal(bitmap, posX, posY, bitmap.getWidth, bitmap.getHeight, false)
    }
    
    /** TODO: scaladoc
      */
    def blit(bitmap: Bitmap, posX: Int, posY: Int, insertWidth: Int,
            insertHeight: Int) {
        // TODO: why not pass through insertWidth and insertHeight?
        blitInternal(bitmap, posX, posY, bitmap.getWidth, bitmap.getHeight, false)
    }
    
    /** TODO: scaladoc
      */
    def transparencyBlit(bitmap: Bitmap, posX: Int, posY: Int) {
        blitInternal(bitmap, posX, posY, bitmap.getWidth, bitmap.getHeight, true)
    }
    
    /** TODO: scaladoc
      */
    def transparencyBlit(bitmap: Bitmap, posX: Int, posY: Int, insertWidth: Int,
            insertHeight: Int) {
        // TODO: why not pass through insertWidth and insertHeight?
        blitInternal(bitmap, posX, posY, bitmap.getWidth, bitmap.getHeight, true)
    }
    
    /** TODO: scaladoc
      */
    protected def blitInternal(bitmap: Bitmap, posX: Int, posY: Int,
            insertWidth: Int, insertHeight: Int, withTransparency: Boolean) {
        // find top left point for blit
        var TLx = if(posX < 0) 0 else posX
        var TLy = if(posY < 0) 0 else posY
        
        // find bottom right point for blit
        var BRx = if(posX + insertWidth > width) width else (posX + insertWidth)
        var BRy = if(posY + insertHeight > height) height else (posY + insertHeight)
        
        // change points to fit range (if set)
        if(rangeMode != null && rangeMode == BitmapRangeMode.ONLY) {
            TLx = if(TLx < rangeTL._1) rangeTL._1 else TLx
            TLy = if(TLy < rangeTL._2) rangeTL._2 else TLy
            BRx = if(BRx > rangeBR._1) rangeBR._1 else BRx
            BRy = if(BRy > rangeBR._2) rangeBR._2 else BRy
        }
        
        // now blit
        val lineWidth = BRx - TLx
        (TLy until BRy).foreach(y => {
            val lineStartOffset = (y * width) + TLx
            val bitmapOffset = (y - posY) * bitmap.getWidth + (TLx - posX)
            (0 until lineWidth).foreach(x => {
                var col = bitmap.getPixels(bitmapOffset + x)
                if(col < 0 || withTransparency) {
                    val a = (col >> 24) & 0xff
                    if(a != 0) {
                        if(withTransparency && a < 0xff) {
                            val bgColor = pixels(lineStartOffset + x)
                            col = blitPixelColor(bgColor, col, a)
                        }
                    }
                    pixels(lineStartOffset + x) = col
                }
            })
        })
    }
    
    /** TODO: scaladoc
      */
    def fill(x: Int, y: Int, fillWidth: Int, fillHeight: Int, color: Int) {
        fillInternal(x, y, fillWidth, fillHeight, color, false)
    }
    
    /** TODO: scaladoc
      */
    def transparencyFill(x: Int, y: Int, fillWidth: Int, fillHeight: Int,
            color: Int) {
        fillInternal(x, y, fillWidth, fillHeight, color, true)
    }
    
    /** TODO: scaladoc
      */
    protected def fillInternal(x: Int, y: Int, fillWidth: Int,
            fillHeight: Int, color: Int, withTransparency: Boolean) {
        // do nothing if completely transparent
        val a = (color >> 24) & 0xff
        if(withTransparency && a == 0) {
            return;
        }
        
        // find top left point for fill
        val TLx = if(x < 0) 0 else x
        val TLy = if(y < 0) 0 else y
        
        // find bottom right point for fill
        val BRx = if(x + fillWidth > width) width else (x + fillWidth)
        val BRy = if(y + fillHeight > height) height else (y + fillHeight)
        
        // now fill
        val effectiveFillWidth = BRx - TLx
        (TLy until BRy).foreach(y => {
            val lineStartOffset = y * width + TLx
            (0 until effectiveFillWidth).foreach(x => {
                var col = color
                if(withTransparency && a < 0xff) {
                    val bgColor = pixels(lineStartOffset + x)
                    col = blitPixelColor(bgColor, col, a)
                }
            })
        })
    }
    
    /** TODO: scaladoc
      */
    protected def blitPixelColor(bgColor: Int, color: Int, alpha: Int): Int = {
        val bgR = bgColor & 0xff0000
        val bgG = bgColor & 0xff00
        val bgB = bgColor & 0xff
        val colR = color & 0xff0000
        val colG = color & 0xff00
        val colB = color & 0xff
        val a = 256 - alpha
        val red = ((colR * alpha + bgR * a) >> 8) & 0xff0000
        val green = ((colG * alpha + bgG * a) >> 8) & 0xff00
        val blue = ((colB * alpha + bgB * a) >> 8) & 0xff
        0xff000000 | red | green | blue
    }
    
}