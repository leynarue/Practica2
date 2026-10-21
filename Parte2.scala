object Parte2 {
  
// 1.Clase Pelicula y su Objeto Acompañante
class Pelicula(
  val titulo: String,
  val duracionMinutos: Int,
  val clasificacion: String
)

object Pelicula {
  def apply(
    titulo: String,
    duracionMinutos: Int,
    clasificacion: String
  ): Pelicula = {
    // Nos encargamos de validar si la duración es <= 0, se usa 90 minutos
    val duracionValidada: Int = if (duracionMinutos <= 0) 90 else duracionMinutos
    new Pelicula(titulo, duracionValidada, clasificacion)
  }
}

// 2.Clase FuncionCine y su Objeto Acompañante
class FuncionCine(
  val pelicula: Pelicula,
  val sala: Int,
  val hora: String,
  val precioEntrada: Double,
  val asientosDisponibles: Int
) {
  // 3. Método puedeReservar
  def puedeReservar(cantidad: Int): Boolean = {
    cantidad <= asientosDisponibles
  }

  // 4. Método costoReserva
  def costoReserva(cantidad: Int): Double = {
    cantidad.toDouble * precioEntrada
  }

  // 5. Método reservar (devuelve un nuevo objeto sin mutar el actual)
  def reservar(cantidad: Int): FuncionCine = {
    if (puedeReservar(cantidad)) {
      new FuncionCine(
        pelicula = this.pelicula,
        sala = this.sala,
        hora = this.hora,
        precioEntrada = this.precioEntrada,
        asientosDisponibles = this.asientosDisponibles - cantidad
      )
    } else {
      this // Si no hay suficientes asientos nos devuelve la misma instancia sin cambios
    }
  }

  // 6. Método descripcion
  def descripcion: String = {
    s"${pelicula.titulo}\nSala: $sala\nHora: $hora\nPrecio: $precioEntrada Bs\nAsientos disponibles: $asientosDisponibles"
  }
}

object FuncionCine {
  def apply(
    pelicula: Pelicula,
    sala: Int,
    hora: String,
    precioEntrada: Double,
    asientosDisponibles: Int
  ): FuncionCine = {
    // Validaciones requeridas
    val salaValida: Int = if (sala >= 1) sala else 1
    val precioValidado: Double = if (precioEntrada >= 0.0) precioEntrada else 0.0
    val asientosValidos: Int = if (asientosDisponibles >= 0) asientosDisponibles else 0

    new FuncionCine(pelicula, salaValida, hora, precioValidado, asientosValidos)
  }

  // 7. Método de fábrica adicional: funcionEstandar
  def funcionEstandar(
    pelicula: Pelicula,
    sala: Int,
    hora: String
  ): FuncionCine = {
    FuncionCine(
      pelicula = pelicula,
      sala = sala,
      hora = hora,
      precioEntrada = 30.0,
      asientosDisponibles = 50
    )
  }
}

def main(args: Array[String]): Unit = {
    println("Pruebas del Sistema de Cine")
    
    val pelicula1: Pelicula = Pelicula(
      titulo = "Dune",
      duracionMinutos = 155,
      clasificacion = "PG-13"
    )

    val funcion1: FuncionCine = FuncionCine(
      pelicula = pelicula1,
      sala = 3,
      hora = "20:30",
      precioEntrada = 35.0,
      asientosDisponibles = 40
    )

    val funcion2: FuncionCine = FuncionCine.funcionEstandar(
      pelicula = pelicula1,
      sala = 1,
      hora = "16:00"
    )

    println("\nDescripción de la función 1")
    println(funcion1.descripcion)

    println("\nPuede reservar 5 entradas?")
    println(funcion1.puedeReservar(5))

    println("\nCosto de reservar 5 entradas")
    println(s"${funcion1.costoReserva(5)} Bs")

    println("\nPrueba de reserva e inmutabilidad}")
    val funcionReservada: FuncionCine = funcion1.reservar(5)
    
    println(funcion1.asientosDisponibles)         
    println(funcionReservada.asientosDisponibles)   
  }
}
