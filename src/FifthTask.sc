val filePath =
  "D:\\Documents\\ScalaProjects\\ScalaDruTest-1\\.myData\\dataTask5.txt"

def longSum(top: List[Int], bottom: List[Int]): List[Int] = {
  @annotation.tailrec
  def loopLongSum(i: Int, buffer: Int, res: List[Int]): List[Int] = {
    val topVal = if (i >= top.length) 0 else top(top.length - i - 1)
    val bottomVal =
      if (i >= bottom.length) 0 else bottom(bottom.length - i - 1)

    if (i < (top.length max bottom.length)) {
      val sum = topVal + bottomVal + buffer
      loopLongSum(i + 1, sum / 10, (sum % 10) :: res)
    } else {
      if (buffer > 0) buffer :: res else res
    }
  }
  loopLongSum(0, 0, Nil)
}

val bufferedSource = io.Source.fromFile(filePath)
val dataLines = bufferedSource.getLines

@annotation.tailrec
def sum(answer: List[Int], line: String): List[Int] = {
  if (!dataLines.hasNext) {
    val intArr = line.toList.map(x => x.toInt - '0'.toInt)
    longSum(answer, intArr)
  } else {
    val intArr = line.toList.map(x => x.toInt - '0'.toInt)
    sum(longSum(answer, intArr), dataLines.next)
  }
}

sum(List(0), dataLines.next).take(10).mkString("<", "", ">")

bufferedSource.close