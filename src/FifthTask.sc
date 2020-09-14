val filePath =
  "D:\\Documents\\ScalaProjects\\ScalaDruTest-1\\.myData\\dataTask5.txt"

val bufferedSource = io.Source.fromFile(filePath)
val dataLines = bufferedSource.getLines

@annotation.tailrec
def sum(answer: BigInt, line: String): BigInt = {
  if (!dataLines.hasNext) answer + BigInt(line)
  else
    sum(answer + BigInt(line), dataLines.next)
}
sum(0, dataLines.next).toString.take(10)

bufferedSource.close