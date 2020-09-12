val dataList = List(-11, -10, -9, -8, -6, -5, -4, -4, -2, -2, -1, -1, -1, 0,
  0, 1, 1, 1, 2, 3, 4, 5, 5, 6, 7, 8, 13, 14)
val Inf = 100
dataList.sorted

@annotation.tailrec
def getMiddleElement(
    items: List[Int],
    prevValue: Int,
    sum: Int,
    lValue: Int,
    rValue: Int
): Int =
  items match {
    case `prevValue` :: tail =>
      getMiddleElement(tail, prevValue, sum, lValue, rValue)
    case head :: tail if (head + lValue + rValue == 0) =>
      getMiddleElement(tail, head, sum + 1, lValue, rValue)
    case head :: tail => getMiddleElement(tail, head, sum, lValue, rValue)
    case _            => sum
  }

@annotation.tailrec
def getRightElement(
    items: List[Int],
    prevValue: Int,
    sum: Int,
    lValue: Int
): Int =
  items match {
    case 0 :: _              => sum
    case `prevValue` :: tail => getRightElement(tail, prevValue, sum, lValue)
    case head :: tail =>
      getRightElement(
        tail,
        head,
        sum + getMiddleElement(tail, Inf, 0, lValue, head),
        lValue
      )
  }

@annotation.tailrec
def buildCombination(items: List[Int], prevValue: Int, sum: Int): Int =
  items match {
    case 0 :: _              => sum
    case `prevValue` :: tail => buildCombination(tail, prevValue, sum)
    case head :: tail =>
      buildCombination(
        tail,
        head,
        sum + getRightElement(tail.reverse, Inf, 0, head)
      )
  }

buildCombination(dataList, Inf, 0)