def palindromeCheck(binNumber: String): Boolean = binNumber == binNumber.reverse

val res = LazyList
  .from(0)
  .map(x => if (palindromeCheck(x.toBinaryString)) x else 0)
  .filter(_ > 0)

res.take(73).sum