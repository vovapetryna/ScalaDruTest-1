val encryptedData =
  "GHMABGZ VKXTMXL LNVVXLL EBDX GHG-LMHI, XGMANLBTLMBV XYYHKM"
val dictionary = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
val punctuation = ".,:;- "

for (shift <- 1 until dictionary.length) {
  println(
    encryptedData.map(c =>
      c match {
        case _ if punctuation contains c => c
        case _ if (dictionary.indexOf(c) - shift) < 0 =>
          dictionary(dictionary.indexOf(c) - shift + dictionary.length)
        case _ =>
          dictionary(dictionary.indexOf(c) - shift)
      }
    )
  )
}
