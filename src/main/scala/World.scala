import scala.collection.mutable.ArrayBuffer

object Clock {
  var current_time = 0;

}

trait Element {
  var coordinates: Coordinates
  var strValue: String
  def updateCoordinates(x: Int, y: Int): Unit = {
    coordinates = Coordinates(x, y)
  }
}

class FutureEventList {
  var list = List[Event]()
}

object Map {
  val size: Int = 20
  def reset: Array[Array[String]] = {
    val newMatrix = Array.ofDim[String](size, size)
    for (r <- 0 to size - 1) {
      for (c <- 0 to size - 1){
        newMatrix(r)(c) = "•"
      }
    }
    return newMatrix
  }
  var matrix: Array[Array[String]] = reset

  def print(): Unit = {
    matrix.foreach(x => println(x.foldLeft(""){(z, i) => z + " " + i}))
  }

  def update(elems: List[Element]) = {
    val newMatrix = reset
    def insertElemPosition(elem: Element) = {
      if (newMatrix(elem.coordinates.x)(elem.coordinates.y) == "•") {
        newMatrix(elem.coordinates.x)(elem.coordinates.y) = elem.strValue
      } else {
        newMatrix(elem.coordinates.x)(elem.coordinates.y) += elem.strValue
      }
    }
    elems.foreach(insertElemPosition(_))
    matrix = newMatrix
  }
}

object DroneState extends Enumeration {
  val Flying: DroneState.Value = Value
  val Landed: DroneState.Value = Value
}

case class Coordinates(val x: Int, val y: Int)

class Station(x: Int, y: Int) extends Element {
  var coordinates: Coordinates = Coordinates(x, y)
  var strValue: String = "S"
}

class Target(x: Int, y: Int) extends Element {
  var coordinates: Coordinates = Coordinates(x, y)
  var strValue: String = "T"
}


class Drone(val start: Station, val target: Target) extends Element {

  object AscisseDirection extends Enumeration {
    val Right = Value(1)
    val Left = Value(-1)
  }

  object OrdinateDirection extends Enumeration {
    val Up = Value(1)
    val Down = Value(-1)
  }

  val state: DroneState.Value = DroneState.Landed
  var coordinates: Coordinates = start.coordinates
  var strValue: String = "D"

  var ascisseDirection: AscisseDirection.Value = if (ascissDistance >= 0) AscisseDirection.Right else AscisseDirection.Left
  var ordinateDirection: OrdinateDirection.Value = if (ordinaDistance >= 0) OrdinateDirection.Up else OrdinateDirection.Down

  def ascissDistance: Int = target.coordinates.x - coordinates.x
  def ordinaDistance: Int = target.coordinates.y - coordinates.y

  def clock() = {
    if (state == DroneState.Flying) {
      var x = coordinates.x
      var y = coordinates.y
      if (ascissDistance != 0) x * ascisseDirection.id
      if (ordinaDistance != 0) y * ordinateDirection.id
      updateCoordinates(x, y)
    }
  }

}

trait Event {

}

case class timeEvent() extends Event
case class windEvent() extends Event


trait Controller[A] {
  def Notify(event: Event)
  def Register(elem: A): Unit
  def Unregister(elem: A): Unit
}

object DroneController extends  Controller[Drone] {
  val drone_list: ArrayBuffer[Drone] = ArrayBuffer[Drone]()
  override def Notify(event: Event): Unit = {
    event match {
      case timeEvent() => drone_list.foreach(_.clock())
    }
  }

  override def Register(drone: Drone): Unit = {
    drone_list += drone
  }

  override def Unregister(drone: Drone): Unit = {
    drone_list -= drone
  }
}

object WorldController extends  Controller[Element] {
  val element_list: ArrayBuffer[Element] = ArrayBuffer[Element]()
  override def Notify(event: Event): Unit = {

  }

  override def Register(elem: Element): Unit = {
    element_list += elem
  }

  override def Unregister(elem: Element): Unit = {
    element_list -= elem
  }
}


object World extends App {
  val emulation_tme = 100
  val start = new Station(5, 5)
  val target = new Target(15, 15)
  val drone1 = new Drone(start, target)
  // this is the main loop
  for (i <- 1 to emulation_tme) {
    print("Hello")
  }
  Map.update(List(start, target, drone1))
  Map.print()
}
