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

case class Station(x: Int, y: Int) extends Element {
  var coordinates: Coordinates = Coordinates(x, y)
  var strValue: String = "S"
}

case class Target(x: Int, y: Int) extends Element {
  var coordinates: Coordinates = Coordinates(x, y)
  var strValue: String = "T"
}


case class Drone(val start: Station, val target: Target) extends Element {

  object AscisseDirection extends Enumeration {
    val Right = Value(1)
    val Left = Value(-1)
  }

  object OrdinateDirection extends Enumeration {
    val Up = Value(1)
    val Down = Value(-1)
  }

  var state: DroneState.Value = DroneState.Landed
  var coordinates: Coordinates = start.coordinates
  var strValue: String = "D"

  var ascisseDirection: AscisseDirection.Value = if (ascissDistance >= 0) AscisseDirection.Right else AscisseDirection.Left
  var ordinateDirection: OrdinateDirection.Value = if (ordinaDistance >= 0) OrdinateDirection.Up else OrdinateDirection.Down

  def ascissDistance: Int = target.coordinates.x - coordinates.x
  def ordinaDistance: Int = target.coordinates.y - coordinates.y

  def updateState(state: DroneState.Value): Unit = this.state = state

  def clock(): Unit = {
    if (state == DroneState.Flying) {
      var x = coordinates.x
      var y = coordinates.y
      if (ascissDistance != 0) x +  1 * ascisseDirection.id
      if (ordinaDistance != 0) y +  1 * ordinateDirection.id
      updateCoordinates(x, y)
    }
  }

}

trait Event {

}

case class TimeEvent() extends Event
case class windEvent() extends Event
case class FireEvent() extends Event


trait Controller[A] {
  def Notify(event: Event): Unit
  def Register(elem: A): Unit
  def Unregister(elem: A): Unit
  def enqueueEvent(event: Event): Unit
  def enqueueEventList(events: ArrayBuffer[Event]): Unit
}

object DroneController extends  Controller[Drone] {
  val drone_list: ArrayBuffer[Drone] = ArrayBuffer[Drone]()
  val event_list: ArrayBuffer[Event] = ArrayBuffer[Event]()

  override def Notify(event: Event): Unit = {
    event match {
      case TimeEvent() => drone_list.foreach(_.clock())
      case FireEvent() => drone_list.foreach(_.updateState(DroneState.Flying))
    }
  }

  override def Register(drone: Drone): Unit = {
    drone_list += drone
  }

  override def Unregister(drone: Drone): Unit = {
    drone_list -= drone
  }

  override def enqueueEvent(event: Event): Unit = {
    event_list += event
  }

  override def enqueueEventList(events: ArrayBuffer[Event]): Unit = {
    events.foreach(event_list += _)
  }
}

object Generator {
  var futureEventList: ArrayBuffer[Event] = ArrayBuffer[Event]()
  var everyWhen: Int = 10
  def cleanEventList: Unit = {
    futureEventList.clear()
  }
  def generateTimeEvent(time: Int): Unit = {
    if ((time % everyWhen) == 0) {
      futureEventList += new TimeEvent
    }
  }
}

object WorldController {
  val element_list: ArrayBuffer[Drone] = ArrayBuffer[Drone]()
  def checkEnd: Boolean = {
      for (elm <- element_list) {
        if (elm.coordinates.x == elm.target.coordinates.x && elm.coordinates.y == elm.coordinates.y) {
           true
        }
      }
    false
  }

  def Register(elem: Drone): Unit = {
    element_list += elem
  }

  def Unregister(elem: Drone): Unit = {
    element_list -= elem
  }
}


object World extends App {
  val emulation_tme = 100
  val start = new Station(5, 5)
  val target = new Target(15, 15)
  val drone1 = new Drone(start, target)
  DroneController.Register(drone1)
  WorldController.Register(drone1)
  val futureEventList: ArrayBuffer[Event] = ArrayBuffer[Event]()
  futureEventList += new FireEvent
  // this is the main loop
  for (time <- 1 to emulation_tme) {
    Generator.cleanEventList
    Generator.generateTimeEvent(time)
    DroneController.enqueueEventList(Generator.futureEventList)
    DroneController.notify()
    futureEventList.clear()
    WorldController.checkEnd
  }
  Map.update(List(start, target, drone1))
  Map.print()
}
