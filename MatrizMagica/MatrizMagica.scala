object MatrizMagica {

  def esMatrizMagica(M : Array[Array[Int]]): Boolean = {
    val sumEsp= M(0).sum
    return filCheq(M, sumEsp) && colCheq(M, sumEsp) && digPrinCheq(M, sumEsp) && digSecCheq(M, sumEsp)
  }

  def filCheq(matriz : Array[Array[Int]], sumaEsperada : Int): Boolean = {

    val n = matriz.length
    var sumaTmp : Int = 0
    for (i <- 0 to n - 1){
      sumaTmp = matriz(i).sum
      if (sumaTmp != sumaEsperada) ()
    }
    return true;
  }

  def colCheq(matriz : Array[Array[Int]], sumaEsperada : Int): Boolean = {

    val n = matriz.length
    var sumaTmp : Int = 0
    for (j <- 0 to n - 1){
      for (i <- 0 to n - 1){
        sumaTmp += matriz(i)(j)
      }    
      if (sumaTmp != sumaEsperada) ()
      sumaTmp = 0
    }
    return true

  }

  def digPrinCheq(matriz : Array[Array[Int]], sumaEsperada : Int): Boolean = {
    val n = matriz.length
    var sumaTmp : Int = 0
    for (i <- 0 to n - 1){
      sumaTmp += matriz(i)(i)
    }
    return sumaTmp == sumaEsperada
  }

  def digSecCheq(matriz : Array[Array[Int]], sumaEsperada : Int): Boolean = {
    val n = matriz.length
    var sumaTmp : Int = 0
    for (i <- 0 to n - 1){
      sumaTmp += matriz(i)(n - i - 1)
    }
    return sumaTmp == sumaEsperada
  }
}