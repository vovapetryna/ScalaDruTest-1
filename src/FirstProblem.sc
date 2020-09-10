val encryptedData = "GHMABGZ VKXTMXL LNVVXLL EBDX GHG-LMHI, XGMANLBTLMBV XYYHKM"
val dictionary = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
val punctuation = ".,:;- "
val dictionaryLength = dictionary.length
val shift = 0

def decryptString(a: String, shift: Int):String = {
  def decryptChar(a: Char):Char = {
    val encryptedChar = (dictionary.indexOf(a) - shift) % dictionaryLength
    if (encryptedChar >= 0) dictionary(encryptedChar) else
      dictionary(dictionaryLength + encryptedChar)
  }

  val phraseLength = a.length

  @annotation.tailrec
  def loopDecryptString(i: Int, decryptedString:String):String = {
    if (i == phraseLength)
      decryptedString
    else {
      if (punctuation.contains(a(i))) {
        loopDecryptString(i + 1, decryptedString + a(i))
      } else
        loopDecryptString(i + 1, decryptedString + decryptChar(a(i)))
    }
  }
  loopDecryptString(0, "")
}

def decrypt(a: String, limit: Int):Unit = {

  @annotation.tailrec
  def loopDecrypt(i: Int):Unit = {
    if (i < limit) {
      println(decryptString(a, i))
      loopDecrypt(i + 1)
    }
  }
  loopDecrypt(1)
}


decrypt(encryptedData, 100)