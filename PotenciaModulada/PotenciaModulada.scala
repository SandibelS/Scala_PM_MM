object PotenciaModulada {
  
  def potMod(a : Int, b: Int, c: Int): Int = {

    if(a < 0) throw new IllegalArgumentException("El entero 'a' debe ser mayor o igual a cero")

    if(b < 0) throw new IllegalArgumentException("El entero 'b' debe ser mayor o igual a cero")

    if(c < 2) throw new IllegalArgumentException("El entero 'c' debe ser mayor o igual a dos")

    if(b == 0) 1
    else ((a % c) * potMod(a, b - 1, c)) % c 
  }
}