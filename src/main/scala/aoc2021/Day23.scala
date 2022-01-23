package aoc2021

import scala.annotation.tailrec
import scala.collection.mutable

case class Day23(state:String, cost:Int){
  // a_b_c_d_e  - the map of the game
  //  f h j l
  //  g i k m

  /**
    * Underrated distance of the actual state from the solution
    * @return distance
    */
  def distance:Int = {
    /** where are this squares? */
    def coordinates = Map('a'->(-4,0),'b'->(-2,0),'c'->(0,0),'d'->(2,0),'e'->(4,0),
      'f'->(-3,1),'g'->(-3,2),'h'->(-1,1),'i'->(-1,2),'j'->(1,1),'k'->(1,2),'l'->(3,1),'m'->(3,2))
    /**
      * Calculate the Manhattan distance
      * @param xy first position
      * @param uv second position
      * @return distance
      */
    def manhattan(xy:(Int,Int), uv:(Int,Int)):Int = (xy._1 - uv._1).abs + (xy._2 - uv._2).abs

    val pos_a1 = coordinates(state.charAt(0))
    val a1 = manhattan(pos_a1,coordinates('f')) min manhattan(pos_a1,coordinates('g'))
    val pos_a2 = coordinates(state.charAt(1))
    val a2 = manhattan(pos_a2,coordinates('f')) min manhattan(pos_a2,coordinates('g'))
    val pos_b1 = coordinates(state.charAt(2))
    val b1 = manhattan(pos_b1,coordinates('h')) min manhattan(pos_b1,coordinates('i'))
    val pos_b2 = coordinates(state.charAt(3))
    val b2 = manhattan(pos_b2,coordinates('h')) min manhattan(pos_b2,coordinates('i'))
    val pos_c1 = coordinates(state.charAt(4))
    val c1 = manhattan(pos_c1,coordinates('j')) min manhattan(pos_c1,coordinates('k'))
    val pos_c2 = coordinates(state.charAt(5))
    val c2 = manhattan(pos_c2,coordinates('j')) min manhattan(pos_c2,coordinates('k'))
    val pos_d1 = coordinates(state.charAt(6))
    val d1 = manhattan(pos_d1,coordinates('l')) min manhattan(pos_d1,coordinates('m'))
    val pos_d2 = coordinates(state.charAt(7))
    val d2 = manhattan(pos_d2,coordinates('l')) min manhattan(pos_d2,coordinates('m'))
    a1+a2+(b1+b2)*10+(c1+c2)*100+(d1+d2)*1000
  }
  /** base of the order */
  val totalCost:Int = this.distance+this.cost
  def path = Map("af"->"", "bf"->"","bh"->"","ch"->"","cj"->"","dj"->"","dl"->"","el"->"",
                 "ag"->"f","bg"->"f","bi"->"h","ci"->"h","ck"->"j","dk"->"j","dm"->"l","em"->"l",
                 "ah"->"b","ai"->"bh","aj"->"bc","ak"->"bcj","al"->"bcd","am"->"bcdl",
                 "bj"->"c","bk"->"cj","bl"->"cd","bm"->"cdl",
                 "cf"->"b","cg"->"bf","cl"->"d","cm"->"dl",
                 "df"->"bc","dg"->"bcf","dh"->"c","di"->"ch",
                 "ef"->"bcd","eg"->"bcdf","eh"->"cd","ei"->"cdh","ej"->"d","ek"->"dj")
  def length =  Map("af"->2, "bf"->2,"bh"->2,"ch"->2,"cj"->2,"dj"->2,"dl"->2,"el"->2,
                    "ag"->3,"bg"->3,"bi"->3,"ci"->3,"ck"->3,"dk"->3,"dm"->3,"em"->3,
                    "ah"->4,"bj"->4,"cl"->4,"cf"->4,"dh"->4,"ej"->4,
                    "ai"->5,"bk"->5,"cm"->5,"cg"->5,"di"->5,"ek"->5,
                    "aj"->6,"bl"->6,"df"->6,"eh"->6,
                    "ak"->7,"bm"->7,"dg"->7,"ei"->7,
                    "al"->8,"ef"->8, "am"->9,"eg"->9)
  def targets(fromNode:Char, i:Int):String = if ("abcde".contains(fromNode)) i match {
    case 0 => "fg"; case 1 => "fg"
    case 2 => "hi"; case 3 => "hi"
    case 4 => "jk"; case 5 => "jk"
    case 6 => "lm"; case 7 => "lm"
  } else "abcde"
  /**
    * It is possible to move from point A to point B
    * @param a start position
    * @param b goal position
    * @return the path is empty?
    */

  def orderPair(i:Int) = {
    val a = state.charAt(i)
    val b = state.charAt(i+1)
    if (a<b) s"$a$b" else s"$b$a"
  }
  def normalized:String = s"${orderPair(0)}${orderPair(2)}${orderPair(4)}${orderPair(6)}"
  private def free(a:Char,b:Char):Boolean = {
    @tailrec
    def inner(s:String):Boolean = {
      if (s.isEmpty) true
      else if (state contains s(0)) false
      else inner(s.drop(1))
    }
    if (state contains b) false
    else if (a < b) inner(path(s"$a$b")) else  inner(path(s"$b$a"))
  }
  private def step(fromNode:Char,toNode:Char,i:Int)= {
    val l = if (fromNode<toNode) length(s"$fromNode$toNode") else length(s"$toNode$fromNode")
    val m = List(1,1,10,10,100,100,1000,1000)
    l*m(i)
  }

  /**
    * The successor states of the actual state
    * @return the 'list' of successors
    */
  def successors:IndexedSeq[Day23] = for (i <- state.indices;
                                          fromNode = state.charAt(i);
                                          toNode <- targets(fromNode,i)
                                          if free(fromNode,toNode))
                                          yield Day23(state.take(i) ++ (toNode +: state.drop(i+1)),cost+step(fromNode,toNode,i))

  override def toString:String = s"$state-$totalCost ($cost/$distance)"

}
object Day23 extends App {

  //val startPosition = "gmfjhkil" // sample problem
  val startPosition = "jkhmfgil" // my problem

  var s = Day23(startPosition,0)
  var openClosed=Set[String](s.normalized)
  val pq: mutable.PriorityQueue[Day23] = mutable.PriorityQueue(s)(Ordering.by(z => -z.totalCost))
  do {
    s = pq.dequeue()
    //print(s" ${s.totalCost} ")
    for (succ <- s.successors if !openClosed.contains(succ.normalized)) {
      pq.enqueue(succ)
      openClosed += succ.normalized
    }
  } while (s.distance > 0)

  println(s"Part 1 ${s.cost}")
}
