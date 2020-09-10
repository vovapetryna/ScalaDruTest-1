val dataList = Array(-11, -10, -9, -8, -6, -5, -4, -4, -2, -2, -1, -1, -1, 0, 0, 1, 1, 1, 2, 3, 4, 5, 5, 6, 7, 8, 13, 14)
val bound = dataList.length - 1
var targetNumber = 0
val Inf = 100

dataList.sorted

@annotation.tailrec
def getMiddleElement(leftId: Int, rightId: Int, midId: Int,
                     prevMidVal: Int, mSum: Int): Int = {
  if (midId == rightId){
    mSum
  }else if (dataList(midId) == prevMidVal){
    getMiddleElement(leftId, rightId, midId + 1, dataList(midId), mSum)
  }else{
    if (dataList(leftId) +
        dataList(midId) +
        dataList(rightId) == 0){
      println(dataList(leftId) + " " +
              dataList(midId) + " " +
              dataList(rightId))

      getMiddleElement(leftId, rightId, midId + 1, dataList(midId), mSum + 1)
    }else{
      getMiddleElement(leftId, rightId, midId + 1, dataList(midId), mSum)
    }
  }
}

@annotation.tailrec
def getRightElement(leftId: Int, rightId: Int, prevRightVal: Int, rSum: Int): Int = {
  if (dataList(rightId) == 0 || leftId == rightId){
    rSum
  }else if (dataList(rightId) == prevRightVal){
    getRightElement(leftId, rightId - 1, dataList(rightId), rSum)
  }else{
    getRightElement(leftId, rightId - 1, dataList(rightId),
      rSum + getMiddleElement(leftId, rightId, leftId + 1, Inf, 0))
  }
}

@annotation.tailrec
def getLeftElement(leftId: Int, prevLeftVal: Int, lSum: Int): Int = {
  if (dataList(leftId) == 0) {
    lSum
  }else if (dataList(leftId) == prevLeftVal){
    getLeftElement(leftId + 1, dataList(leftId), lSum)
  }else{
    getLeftElement(leftId + 1, dataList(leftId),
      lSum + getRightElement(leftId, bound, Inf, 0))
  }
}



getLeftElement(0, Inf, 0)