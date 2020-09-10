val filePath = "D:\\Documents\\ScalaProjects\\AlgoTasks\\src\\dataTask5.txt"

def longSum(top: Array[Int], bottom: Array[Int]): Array[Int] = {

  @annotation.tailrec
  def loopLongSum(i: Int, buffer: Int, res: Array[Int]): Array[Int] = {
    val topVal = if (i >= top.length) 0 else top(top.length - i - 1)
    val bottomVal = if (i >= bottom.length) 0 else bottom(bottom.length - i - 1)

    if (i >= top.length && i >= bottom.length && buffer == 0)
      res
    else if (i >= top.length && i >= bottom.length){
      Array.concat(Array(buffer), res)
    }else{
      val sum = topVal + bottomVal + buffer
      if (sum < 10){
        loopLongSum(i + 1, 0, Array.concat(Array(sum), res))
      }else{
        loopLongSum(i + 1, 1, Array.concat(Array(sum - 10), res))
      }
    }
  }

  loopLongSum(0, 0, Array())
}


val bufferedSource = io.Source.fromFile(filePath)
val dataLines: Array[String] = bufferedSource.getLines.toArray
bufferedSource.close

@annotation.tailrec
def sum(i: Int, answer: Array[Int]):Array[Int] = {
  if (i == dataLines.length)
    answer
  else{
    val intArr = dataLines(i).toArray.map(x => (x.toInt - '0'.toInt))
    sum(i + 1, longSum(answer, intArr))
  }
}

sum(0, Array(0)).take(10).mkString("<", "", ">")