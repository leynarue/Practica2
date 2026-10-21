object Parte1 {
  
  //Ejercicio 1
  val duplicar: Int => Int = x => x * 2
  val cuadrado: Int => Int = x => x * x
  val esPar: Int => Boolean = x => x % 2 == 0

  //Ejercicio 2
  val cubo: Int => Int = x => x * x * x
  val promedio: (Double, Double) => Double = (a, b) => (a + b) / 2.0
  val mayor: (Int, Int) => Int = (a, b) => if (a > b) a else b
  val esMultiploDeTres: Int => Boolean = x => x % 3 == 0

  //Ejercicio 3
  def transformar(numero: Int, operacion: Int => Int): Int = {
    operacion(numero)
  }

  //Ejercicio 4
  def calcular(a: Double, b: Double, operacion: (Double, Double) => Double): Double = {
    operacion(a, b)
  }

  //Ejercicio 5
  def crearMultiplicador(factor: Int): Int => Int = {
    (x: Int) => x * factor
  }

  //Ejercicio 6
  def aplicarDescuento(precio: Double, porcentaje: Double): Double = {
    precio * (1.0 - porcentaje / 100.0)
  }
  val descuento10: Double => Double = aplicarDescuento(_: Double, 10.0)
  val descuento20: Double => Double = aplicarDescuento(_: Double, 20.0)
  val descuento50: Double => Double = aplicarDescuento(_: Double, 50.0)

  //Ejercicio 7
  def elevar(exponente: Int)(base: Int): Int = {
    Math.pow(base.toDouble, exponente.toDouble).toInt
  }
  val cuadradoElem: Int => Int = elevar(2)
  val cuboElem: Int => Int = elevar(3)
  val cuartaPotencia: Int => Int = elevar(4)

  //Ejercicio 8
  val limpiar: String => String = _.trim
  val convertirMinusculas: String => String = _.toLowerCase
  val longitud: String => Int = _.length
  val esLongitudValida: Int => Boolean = _ >= 5
  val validarTexto: String => Boolean = limpiar andThen convertirMinusculas andThen longitud andThen esLongitudValida

  //Ejercicio 9
  def sumar(desde: Int, hasta: Int, transformar: Int => Int): Int = {
    if (desde > hasta) 0
    else transformar(desde) + sumar(desde + 1, hasta, transformar)
  }

  def producto(desde: Int, hasta: Int, transformar: Int => Int): Int = {
    if (desde > hasta) 1
    else transformar(desde) * producto(desde + 1, hasta, transformar)
  }

  //Ejercicio 10
  def transformarTexto(texto: String, transformacion: Char => Char): String = {
    if (texto.isEmpty) ""
    else transformacion(texto.head) + transformarTexto(texto.tail, transformacion)
  }

  def buscarCaracter(texto: String, condicion: Char => Boolean): Boolean = {
    if (texto.isEmpty) false
    else condicion(texto.head) || buscarCaracter(texto.tail, condicion)
  }
 
  def main(args: Array[String]): Unit = {
    println("Ejercicio 1")
    println(s"duplicar(5) = ${duplicar(5)}")
    println(s"cuadrado(4) = ${cuadrado(4)}")
    println(s"esPar(8) = ${esPar(8)}")

    println("\nEjercicio 2")
    println(s"cubo(3) = ${cubo(3)}")
    println(s"promedio(4.0, 6.0) = ${promedio(4.0, 6.0)}")
    println(s"mayor(10, 25) = ${mayor(10, 25)}")
    println(s"esMultiploDeTres(9) = ${esMultiploDeTres(9)}")

    println("\nEjercicio 3")
    println(s"transformar(5, x => x * 2) = ${transformar(5, x => x * 2)}")
    println(s"transformar(5, x => x * x) = ${transformar(5, x => x * x)}")

    println("\nEjercicio 4")
    println(s"calcular(10, 5, (a, b) => a + b) = ${calcular(10, 5, (a, b) => a + b)}")

    println("\nEjercicio 5")
    val duplicarNum = crearMultiplicador(2)
    val triplicar = crearMultiplicador(3)
    val porDiez = crearMultiplicador(10)
    println(s"duplicar(7) = ${duplicarNum(7)}")
    println(s"triplicar(7) = ${triplicar(7)}")
    println(s"porDiez(7) = ${porDiez(7)}")

    println("\nEjercicio 6")
    println(s"descuento10(500) = ${descuento10(500)}")
    println(s"descuento20(500) = ${descuento20(500)}")

    println("\n=== Ejercicio 7 ===")
    println(s"cuadrado(5) = ${cuadradoElem(5)}")
    println(s"cubo(3) = ${cuboElem(3)}")
    println(s"cuartaPotencia(2) = ${cuartaPotencia(2)}")

    println("\nEjercicio 8")
    println(s"validarTexto(\"  Scala  \") = ${validarTexto("  Scala  ")}")
    println(s"validarTexto(\"hi\") = ${validarTexto("hi")}")

    println("\nEjercicio 9")
    println(s"sumar(1, 4, x => x) = ${sumar(1, 4, x => x)}")
    println(s"sumar(1, 3, x => x * x) = ${sumar(1, 3, x => x * x)}")
    println(s"producto(1, 4, x => x) = ${producto(1, 4, x => x)}")

    println("\nEjercicio 10")
    println(s"transformarTexto(\"scala\", c => c.toUpper) = ${transformarTexto("scala", c => c.toUpper)}")
    println(s"buscarCaracter(\"Scala2026\", c => c.isDigit) = ${buscarCaracter("Scala2026", c => c.isDigit)}")
    println(s"buscarCaracter(\"Scala\", c => c == 'z') = ${buscarCaracter("Scala", c => c == 'z')}")
  }
}