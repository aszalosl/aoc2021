package aoc2021

import scala.annotation.tailrec
import scala.collection.mutable

case class Day23b(state:String, cost:Int){
  def weight = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  def weightType = Map(1 -> 1, 2 -> 10, 3 -> 100, 4 -> 1000)
  //def roomSize = 2
  def roomSize = 4
  /*  01x2x3x4x56  hallway
        A B C D    rooms
   */
  /** break the state into hallway and rooms
    * Attention, the split omit the ending empty list!*/
  val innerState:List[String] = (state.split(",").toList ++ List("","","")).take(5)
  /**
    * Which kind of amphipod standing at given position?
    * @param position Id of the position
    * @return type (1-4)
    */
  def ampType(position:Int):Int = innerState.head.charAt(position)-64
  /**
    * The room i is free to enter?
    * @param i index of the room (1-4)
    * @return
    */
  def freeRoom(i:Int):Boolean = innerState(i).isEmpty ||
     (innerState(i).length <= roomSize && innerState(i).forall(_ == "ABCD".charAt(i-1)))
  /**
    * Can we move from the hallway to a room?
    * @param position ID of the position at the hallway, determines the room ID
    * @return the path is free
    */
  def canGoToRoom(position:Int):Boolean = {
    val amphipodType = ampType(position)
    if (amphipodType>=1 && amphipodType<=4 && freeRoom(amphipodType)) {
      if (amphipodType==position || amphipodType+1==position) true else {
        val nextPosition = position + (amphipodType - position).sign
        val b = amphipodType min nextPosition
        val e = amphipodType max nextPosition
        innerState.head.substring(b, e + 1).forall(_ == '.')
      }
    }
    else false
  }
  /**
    * Can we move from the room to a given hallway position?
    * @param roomNumber the ID of the room
    * @param position ID of the position at the hallway
    * @return the path is free
    */
  def canGoToHallway(roomNumber:Int, position:Int):Boolean = {
    if (innerState.head.charAt(position) != '.' || freeRoom(roomNumber)) false else {
      if (roomNumber < position) innerState.head.substring(roomNumber + 1, position + 1).forall(_ == '.')
      else innerState.head.substring(position, roomNumber + 1).forall(_ == '.')
    }
  }
  /**
    * Auxiliary function to construct a successor state
    * @param position the position in the hallway to change
    * @param c the new character at this position
    * @return the modified string
    */
  def replace(position:Int, c:Char):String =
    {innerState.head.take(position) ++ (c +: innerState.head.drop(position+1))}

  /**
    * move one amphipod into its own room
    * @param position position ID in the hallway
    * @return string notation of a successor position
    */
  def goToRoom(position:Int):String = {
    val c = innerState.head.charAt(position)
    val h = replace(position,'.')
    val rA = innerState(1)
    val rB = innerState(2)
    val rC = innerState(3)
    val rD = innerState(4)
     c match {
      case 'A' => s"$h,$c$rA,$rB,$rC,$rD"
      case 'B' => s"$h,$rA,$c$rB,$rC,$rD"
      case 'C' => s"$h,$rA,$rB,$c$rC,$rD"
      case 'D' => s"$h,$rA,$rB,$rC,$c$rD"
    }

  }
  def goToHallway(roomNumber:Int, position:Int): String = {
    val remains = innerState(roomNumber).tail
    val c = innerState(roomNumber).head
    val h = replace(position,c)
    val rA = innerState(1)
    val rB = innerState(2)
    val rC = innerState(3)
    val rD = innerState(4)
    roomNumber match {
      case 1 => s"$h,$remains,$rB,$rC,$rD"
      case 2 => s"$h,$rA,$remains,$rC,$rD"
      case 3 => s"$h,$rA,$rB,$remains,$rD"
      case 4 => s"$h,$rA,$rB,$rC,$remains"
    }
  }
  /**
    * What is the distance of an occupied hallway position and the lowest empty space of a room?
    * (Or the distance of top occupied room position and the empty hallway position?)
    * @param position position ID in the hallway
    * @param roomNumber ID of the room
    * @return distance, i.e. number of steps
    */
  def distance(position:Int, roomNumber:Int):Int = {
    val occupied = if (innerState.head.charAt(position)=='.') 1 else 0
    val inRoom = roomSize-innerState(roomNumber).length
    val inHallway = if (position<=roomNumber) 2*(roomNumber-position)+1 else 2*(position-roomNumber)-1
    val correction = if (position==0 || position==6) 1 else 0
    inRoom+inHallway-correction+occupied
  }

  def successors: List[Day23b] = {
    val goOut = for (position <- 0 to 6;
                     roomNumber <- 1 to 4
                     if canGoToHallway(roomNumber, position)) yield
      Day23b(goToHallway(roomNumber,position), cost +
             distance(position, roomNumber)*weight(innerState(roomNumber).head))
    val goIn = for (position <- 0 to 6;
                    roomNumber = ampType(position)
                     if canGoToRoom(position)) yield {
      Day23b(goToRoom(position), cost + distance(position, roomNumber)*weightType(roomNumber))

    }
    /*
    var goIn = List[Day23b]()
      for (position <- 0 to 6) {
        val roomNumber = ampType(position)
        if (canGoToRoom(position)) {
          println(s"$position - $roomNumber")
          val c = distance(position, roomNumber)*weightType(roomNumber)
          val ns = goToRoom(position)
          goIn = goIn :+ Day23b(ns,c+cost)
        }
      } */
    (goOut ++ goIn).toList
  }

  //override def toString:String
  //= s"h:'${innerState.head}' A:'${innerState(1)}' B:'${innerState(2)}' C:'${innerState(3)}' D:'${innerState(4)}', ($cost)"
}

object Day23b extends App {
  //val startingState = ".......,CC,BD,AA,DB"  //part 1
  val startingState = ".......,CDDC,BCBD,ABAA,DACB"  //part 2

  /**
    * Depth first search (??? minutes)
    * @param sl stack of states
    * @return -1 and writes the possible solutions
    */
  @tailrec
  def dfs(sl:List[Day23b]):Int = {
    //println(s"${sl.head} ... ${sl.length}")
    if (sl.isEmpty) -1
    else {
      val h = sl.head
      val t = sl.tail
      if (h.state==".......,AA,BB,CC,DD") println(h.cost)
      val s = h.successors ++ t
      dfs(s)
    }
  }

  /**
    * branch & bound (13 minutes for part 1)
    * @param sl stack of states
    * @param limit the value of the best solution until now
    * @return the value of the best solution
    */
  @tailrec
  def bb(sl:List[Day23b], limit:Int):Int = {
    //println(s"${sl.head} ... ${sl.length}")
    if (sl.isEmpty) limit
    else {
      val h = sl.head
      if (h.cost >= limit) bb(sl.tail,limit)
      else if (h.state == ".......,AA,BB,CC,DD") {
        print(s"${h.cost}  ")
        bb(sl.tail,h.cost)
      } else {
        val s = h.successors ++ sl.tail
        bb(s, limit)
      }
    }
  }

  /**
    * Uniform cost search
    * @param state starting state
    * @return minimal cost
    */
  def ucs(state: String): Int = {
    var s = Day23b(state, 0)
    var openClosed:mutable.HashMap[String,Int] = mutable.HashMap(state->0)
    val pq: mutable.PriorityQueue[Day23b] = mutable.PriorityQueue(s)(Ordering.by(z => -z.cost))
    do {
      s = pq.dequeue()
      for (succ <- s.successors if succ.cost<openClosed.getOrElse(succ.state,100000)) {
        pq.enqueue(succ)
        openClosed.update(succ.state,succ.cost)
      }
    //} while (s.state != ".......,AA,BB,CC,DD")
    } while (s.state != ".......,AAAA,BBBB,CCCC,DDDD")
    s.cost
  }

  val start = Day23b(startingState, 0)
  //val p1 = dfs(List(start))
  //val p1 = bb(List(start),30000)
  val p1 = ucs(startingState)
  println(p1)
}