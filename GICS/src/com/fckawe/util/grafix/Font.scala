package com.fckawe.util.grafix

/** TODO: scaladoc
 */
class Font(bitmapData: Array[Array[Bitmap]], chars: Array[String], width: Int,
        height: Int, letterSpacing: Short, lineSpacing: Short) {
    
    def this(bitmapData: Array[Array[Bitmap]], chars: Array[String], width: Int,
        height: Int) = this(bitmapData, chars, width, height, 0, 2)
        
    def getChars: Array[String] = chars
    def getWidth: Int = width
    def getHeight: Int = height
    def getLetterSpacing: Short = letterSpacing
    def getLineSpacing: Short = lineSpacing
    
    /** TODO: scaladoc
      */
    def getStringWidth(string: String): Int = string.length() * width
    
    /** TODO: scaladoc
      */
    def getCharBitmap(char: Char): Bitmap = {
        (0 until chars.length).foreach(y => {
            val line = chars(y)
            val x = line.indexOf(char)
            if(x >= 0) {
                bitmapData(x)(y)
            }
        })
        null
    }
    
    /** TODO: scaladoc
      */
    def hasLowercaseLetters(): Boolean = {
        chars.foreach(line => {
            if(line.indexOf('a') >= 0) return true
        })
        return false
    }

}