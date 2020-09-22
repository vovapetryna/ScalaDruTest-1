// DataRoot University DE W4 Quiz #1


//======================================================================================================================
// Task #1
def stringXOR(string: String): Int = string.count(_ == '1') % 2

stringXOR("1 0 0 1 0") == 0
stringXOR("1 0 1 1 1 0 0 1 0 0 0 0") == 1
stringXOR("1 0 0 1 0 1 0 0 1 0 1 0 1 0 1 0 1 0") == 0


//======================================================================================================================
// Task #2
def multiples(n: Int): Int = (1 until n).collect { case x if (x % 3 == 0 || x % 5 == 0) => x }.sum

multiples(10) == 23
multiples(23) == 119
multiples(119) == 3300


//======================================================================================================================
// Task #3
def automorphic(n: Int): Boolean = (n*n).toString.endsWith(n.toString)

automorphic(1)
!automorphic(3)
automorphic(6)
!automorphic(9)
automorphic(25)
!automorphic(53)
automorphic(76)
!automorphic(95)
automorphic(625)
!automorphic(225)


//======================================================================================================================
// Task #4
def simpleIndices(value:String, startIndex: Int):Int =
  value.zipWithIndex.foldLeft((0, -1)) { case ((buffer, ans), (c, id)) => c match {
    case '(' if id >= startIndex => (buffer + 1, ans)
    case ')' if id >= startIndex => (buffer - 1, if (buffer == 1 && ans == -1) id else ans)
    case _ => (buffer, ans)
  }}._2


simpleIndices("((1)23(45))(aB)", 0) == 10
simpleIndices("((1)23(45))(aB)", 1) == 3
simpleIndices("((1)23(45))(aB)", 2) == -1
simpleIndices("((1)23(45))(aB)", 6) == 9
simpleIndices("((1)23(45))(aB)", 11) == 14
simpleIndices("(g(At)IO(f)(tM(qk)YF(n)Nr(E)))", 11) == 28
simpleIndices("(g(At)IO(f)(tM(qk)YF(n)Nr(E)))", 0) == 29
simpleIndices("(>_(va)`?(h)C(as)(x(hD)P|(fg)))", 19) == 22


//======================================================================================================================
// Task #5
def tribonacci(triple: (Int, Int, Int), n: Int): List[Int] = {
  @annotation.tailrec
  def loop(t: (Int, Int, Int), n: Int, ans:List[Int]):List[Int] = n match {
    case 0 => ans.reverse
    case _ => loop((t._2, t._3, t._1 + t._2 + t._3), n - 1, t._1 :: ans)
  }
  loop(triple, n, Nil)
}


tribonacci((1, 1, 1), 10) == List(1, 1, 1, 3, 5, 9, 17, 31, 57, 105)
tribonacci((0, 0, 1), 10) == List(0, 0, 1, 1, 2, 4, 7, 13, 24, 44)
tribonacci((0, 1, 1), 10) == List(0, 1, 1, 2, 4, 7, 13, 24, 44, 81)
tribonacci((1, 0, 0), 10) == List(1, 0, 0, 1, 1, 2, 4, 7, 13, 24)
tribonacci((0, 0, 0), 10) == List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
tribonacci((1, 2, 3), 10) == List(1, 2, 3, 6, 11, 20, 37, 68, 125, 230)
tribonacci((3, 2, 1), 10) == List(3, 2, 1, 6, 9, 16, 31, 56, 103, 190)
tribonacci((1, 1, 1), 1) == List(1)
tribonacci((300, 200, 100), 0) == Nil


//======================================================================================================================
// Task #6
def reverseFun(string: String): String = string.reverse.zip(string)
  .flatten(d => List(d._1, d._2))
  .mkString("", "", "")
  .take(string.length)

reverseFun("012345") == "504132"
reverseFun("jointhedarkside") == "ejdoiisnktrhaed"


//======================================================================================================================
// Task #7
@annotation.tailrec
def foldAnArray(list: List[Int], nTimes: Int): List[Int] = if (nTimes == 0) list else
  foldAnArray(
    (list take list.length/2)
    .zipAll((list drop list.length/2).reverse, 0, 0)
    .map(d => d._1 + d._2), nTimes-1
  )

foldAnArray(List(1, 2, 3, 4, 5), 1) == List(6, 6, 3)
foldAnArray(List(1, 2, 3, 4, 5), 2) == List(9, 6)


//======================================================================================================================
// Task #8
def stringExpansion(string: String): String = {
  string.foldLeft((1, "")){case ((buffer, ans), c) => c match {
    case d if d.isDigit => (c.toInt - '0'.toInt, ans)
    case c => (buffer, ans + (c.toString * buffer))
  }}._2
}

stringExpansion("3abc") == "aaabbbccc"
stringExpansion("3D2a5d2f") == "DDDaadddddff"
stringExpansion("0d0a0v0t0y") == ""
stringExpansion("3d332f2a") == "dddffaa"
stringExpansion("abcde") == "abcde"


//======================================================================================================================
// Task #9
@annotation.tailrec
def lifePathNumber(date: String): Int = if (date.length == 1) date.toInt else
  lifePathNumber(date.map(c => c.asDigit).filter(_ > 0).sum.toString)
  
lifePathNumber("1879-03-14") == 6 // Albert Einstein
lifePathNumber("1815-12-10") == 1 // Ada Lovelace
lifePathNumber("1961-07-04") == 1 // Brendan Eich
lifePathNumber("1955-10-28") == 4 // Bill Gates
lifePathNumber("1452-04-15") == 4 // Leonardo da Vinci
lifePathNumber("1791-12-26") == 2 // Charles Babbage
lifePathNumber("1906-12-09") == 1 // Grace Hopper
lifePathNumber("1912-06-23") == 6 // Alan Turing
lifePathNumber("1950-08-11") == 7 // Steve Wozniak
lifePathNumber("1956-01-31") == 8 // Guido van Rossum
lifePathNumber("1965-04-14") == 3 // Yukihiro Matsumoto
lifePathNumber("1971-06-28") == 7 // Elon Musk


//======================================================================================================================
// Task #10
case class ChromosomePair(chromosome1: List[String], chromosome2: List[String]) {
  def crossover(crossIndices: List[Int]): ChromosomePair = ChromosomePair(
    chromosome1.zipWithIndex
      .map(d => if ((crossIndices :+ chromosome1.length)
        .distinct.sorted.indexWhere(_ > d._2) % 2 == 0) chromosome1(d._2) else chromosome2(d._2)),
    chromosome2.zipWithIndex
      .map(d => if ((crossIndices :+ chromosome1.length)
        .distinct.sorted.indexWhere(_ > d._2) % 2 == 0) chromosome2(d._2) else chromosome1(d._2))
  )
}

ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2")).crossover(Nil) == ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2"))
ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2")).crossover(List(1)) == ChromosomePair(List("1", "2", "2", "2", "2"), List("2", "1", "1", "1", "1"))
ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2")).crossover(List(1, 1)) == ChromosomePair(List("1", "2", "2", "2", "2"), List("2", "1", "1", "1", "1"))
ChromosomePair(List("1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2")).crossover(List(1, 3)) == ChromosomePair(List("1", "2", "2", "1", "1"), List("2", "1", "1", "2", "2"))
ChromosomePair(List("1", "1", "1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2", "2", "2")).crossover(List(1, 3, 5)) == ChromosomePair(List("1", "2", "2", "1", "1", "2", "2"), List("2", "1", "1", "2", "2", "1", "1"))
ChromosomePair(List("1", "1", "1", "1", "1", "1", "1"), List("2", "2", "2", "2", "2", "2", "2")).crossover(List(3, 5, 1)) == ChromosomePair(List("1", "2", "2", "1", "1", "2", "2"), List("2", "1", "1", "2", "2", "1", "1"))


//======================================================================================================================
//Task #11
def fatFingers(string: String): String = {
  @annotation.tailrec
  def loop(i: Int, buffer: Int, ans: String):String = {
    if (i == string.length) ans else
    if ("aA" contains string(i)) loop(i + 1, if (buffer == 0) 1 else -buffer, ans) else
    loop(i+1, buffer, ans + (
      if (buffer == 1) string(i).toUpper else
        if (buffer == -1) string(i).toLower else string(i)
      ))
  }
  loop(0, 0, "")
}

fatFingers("The quick brown fox jumps over the lazy dog.") == "The quick brown fox jumps over the lZY DOG."
fatFingers("aAaaaaAaaaAAaAa") == ""
fatFingers("The end of the institution, maintenance, and administration of government, is to secure the existence of the body politic, to protect it, and to furnish the individuals who compose it with the power of enjoying in safety and tranquillity their natural rights, and the blessings of life: and whenever these great objects are not obtained, the people have a right to alter the government, and to take measures necessary for their safety, prosperity and happiness.") == "The end of the institution, mINTENnce, ND dministrTION OF GOVERNMENT, IS TO SECURE THE EXISTENCE OF THE BODY POLITIC, TO PROTECT IT, nd to furnish the individuLS WHO COMPOSE IT WITH THE POWER OF ENJOYING IN Sfety ND TRnquillity their nTURl rights, ND THE BLESSINGS OF LIFE: nd whenever these greT OBJECTS re not obtINED, THE PEOPLE Hve  RIGHT TO lter the government, ND TO Tke meSURES NECESSry for their sFETY, PROSPERITY nd hPPINESS."


//======================================================================================================================
// Task #12
def xbonacci(signature: List[Int], n: Int): List[Int] = n match {
  case 0 => Nil
  case _ => signature.head :: xbonacci(signature.tail :+ signature.sum, n - 1)
}

xbonacci(List(0, 1), 10) == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
xbonacci(List(1, 1), 10) == List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
xbonacci(List(0, 0, 0, 0, 1), 10) == List(0, 0, 0, 0, 1, 1, 2, 4, 8, 16)
xbonacci(List(1, 0, 0, 0, 0, 0, 1), 10) == List(1, 0, 0, 0, 0, 0, 1, 2, 3, 6)
xbonacci(List(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 20) == List(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 4, 8, 16, 32, 64, 128, 256)
