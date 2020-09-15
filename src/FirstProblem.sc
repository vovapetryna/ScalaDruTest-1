val encryptedData =
  "GHMABGZ VKXTMXL LNVVXLL EBDX GHG-LMHI, XGMANLBTLMBV XYYHKM"
val dictionary = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

for (shift <- 1 until dictionary.length) {
  println(
    encryptedData.map(x => x -> dictionary.indexOf(x)).map {
      case (_, charIdx) if charIdx > -1 && (charIdx - shift) < 0 =>
        dictionary(charIdx - shift + dictionary.length)
      case (_, charIdx) if charIdx > -1 => dictionary(charIdx - shift)
      case (char, _)                    => char
    }.mkString("", "", "")
  )
}
