package aoc2021
import scala.annotation.tailrec
//import scala.io.Source.fromResource

/**
  * state of the deterministic game
  *
  * @param pos1 position of player 1
  * @param pos2 position of player 2
  * @param point1 points of player 1
  * @param point2 points of player 2
  * @param n actual state of the dice
  */
case class State(pos1:Int, pos2:Int, point1:Int, point2:Int, n:Int ) {
  /**
    * based on the rules of the game
    *
    * @return the next state
    * */
  def next:State = {
    val s1 = n+1 + n+2 + n+3
    val s2 = n+4 + n+5 + n+6
    val newPos1 = (pos1+s1)%10
    val newPoint1 = point1 + (if (newPos1==0) 10 else newPos1)
    val newPos2 = (pos2+s2)%10
    val newPoint2 = point2 + (if (newPos2==0) 10 else newPos2)
    if (newPoint1>=1000) State(newPos1,pos2,newPoint1, point2, n+3)
    else State( newPos1, newPos2, newPoint1, newPoint2, n+6)
  }
  /** formatted output of the state */
  override def toString:String =  s" $pos1/$point1, $pos2/$point2 ($n)"
}

/**
  * state of the Dirac game
  *
  * @param pos1 position of player 1
  * @param pos2 position of player 2
  * @param point1 points of player 1
  * @param point2 points of player 2
  * @param next who is next? true-player 1, false - player 2
  */
case class State2(pos1:Int, pos2:Int, point1:Int, point2:Int, next:Boolean, repetition:BigInt ) {
  /**
    * based on the rules of the game
    *
    * @return possible steps and their repetitions
    * */
  val steps = Map(3->1, 4->3, 5->6, 6->7, 7->6, 8->3, 9->1)

  /**
    * where to go?
    *
    * @param position actual position
    * @param step sum of the rolls
    * @return next position
    */
  def nextPlace(position:Int, step:Int) = (position+step)%10

  /**
    * Calculate the new points
    * @param point actual score
    * @param position position of the player
    * @param step sum of the rolls
    * @return new score
    */
  def newPoint(point:Int, position:Int, step:Int) = point + (if ((position+step)%10==0) 10 else (position+step)%10)

  /**
    * what are the successor states?
    * @return list of successor states
    */
  def nexts:List[State2] = {
    if (next)
      steps.map(s => State2(nextPlace(this.pos1,s._1), this.pos2,
                            newPoint(this.point1, this.pos1, s._1), this.point2,
                            false, this.repetition*s._2)).toList
    else
      steps.map(s => State2(this.pos1, nextPlace(this.pos2,s._1),
                            this.point1, newPoint(this.point2, this.pos2, s._1),
                            true, this.repetition*s._2)).toList
  }

  /** formatted output of the state */
  override def toString:String =  if (next)
    s" $pos1/$point1, $pos2/$point2 (1/$repetition)"
    else s" $pos1/$point1, $pos2/$point2 (2/$repetition)"
}

object Day21 extends App {
  /**
    * follow the flow of the game
    *
    * @param s actual state
    * @return final state
    */
  @tailrec
  def game(s: State): State = {
    if (s.point1 >= 1000 || s.point2 >= 1000) s
    else game(s.next)
  }

  def result(s: State): Int =  s.n * (if (s.point1 >= 1000)  s.point2 else s.point1)
  //val starting1 = 4 // sample problem
  //val starting2 = 8
  val starting1 = 2  // my input
  val starting2 = 8

  val s0 = State(starting1,starting2,0,0,0)
  println(s0.toString)
  val sf = game(s0)
  println(sf.toString)
  val r1 = result(sf)
  println(s"Part 1: $r1")
  //--------------------------------------------------------------- Part 2
  val s2 = State2(starting1,starting2,0,0,true,BigInt(1))

  @tailrec
  def game2(l:List[State2], win:BigInt, lose:BigInt):BigInt = {
    //println(l, win, lose)
    if (l.isEmpty) win max lose
    else {
      if (l.head.point1 >= 21) game2(l.tail, win + l.head.repetition, lose)
      else if (l.head.point2 >= 21) game2(l.tail, win, lose + l.head.repetition)
      else {
        val ns = l.head.nexts
        //val remains = join(ns, l.tail)
        game2(ns ++ l.tail, win, lose)
      }
    }
  }
  // TODO: speed up, taking care of repetitions
  val r2 = game2(List(s2), BigInt(0), BigInt(0))
  println(s"Part 2: $r2")
}