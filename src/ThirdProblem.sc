val limit = 73

def palindromeCheck(binNumber: String):Boolean = {
  val binNumberLength = binNumber.length

  @annotation.tailrec
  def loopPalindromeCheck(i: Int): Boolean = {
    if (i == binNumberLength / 2)
      true
    else
      if (binNumber(i) == binNumber(binNumberLength - i - 1))
        loopPalindromeCheck(i + 1)
      else
        false
  }

  loopPalindromeCheck(0)
}

def palindromeNumber(limit: Int): Int = {

  @annotation.tailrec
  def loopPalindromeNumber(i: Int, count: Int, sum:Int): Int = {
    if (count > limit)
      sum
    else
      if (palindromeCheck(i.toBinaryString))
        loopPalindromeNumber(i + 1, count + 1, sum + i)
      else
        loopPalindromeNumber(i + 1, count, sum)
  }

  loopPalindromeNumber(0, 0, 0)
}

palindromeNumber(73)
