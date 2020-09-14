def palindromeCheck(binNumber: List[Char]): Boolean = binNumber == binNumber.reverse

def palindromeNumber(limit: Int): Int = {
  @annotation.tailrec
  def loopPalindromeNumber(number: Int, numberPoly: Int, sum: Int): Int = {
    if (numberPoly > limit)
      sum
    else if (palindromeCheck(number.toBinaryString.toList))
      loopPalindromeNumber(number + 1, numberPoly + 1, sum + number)
    else
      loopPalindromeNumber(number + 1, numberPoly, sum)
  }
  loopPalindromeNumber(0, 0, 0)
}

palindromeNumber(73)