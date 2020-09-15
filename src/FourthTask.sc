val dataList = List(-11, -10, -9, -8, -6, -5, -4, -4, -2, -2, -1, -1, -1, 0,
  0, 1, 1, 1, 2, 3, 4, 5, 5, 6, 7, 8, 13, 14)

dataList.combinations(3).toList.count(_.sum == 0)