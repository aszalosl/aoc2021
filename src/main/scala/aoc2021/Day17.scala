package aoc2021

import scala.annotation.tailrec
import scala.io.Source.fromResource
object Day17 extends App {
  /**
    * Parse the corrdinate data
    *
    * @param s string contains the min..max pair
    * @return (min,max) pair of integers
    */
  def scan(s:String):Tuple2[Int,Int] = {
    val e = s.indexOf('=')
    val p = s.indexOf('.')
    val a = s.substring(e+1,p).toInt
    val b = s.substring(p+2).toInt
    (a,b)
  }
  /**
    * Find the suitable starting x-velocity (in reverse direction)
    *
    * @param d actual distance
    * @param v actual velocity
    * @param xs min-max pair
    * @return the final speed, which will be our starting speed
    */
  @tailrec
  def distanceX(d: Int, v:Int, xs:Tuple2[Int,Int]):Int = {
    if (xs._1<=d && d<=xs._2) v else distanceX(d+v, v+1, xs)
  }
  @tailrec
  def positionY(p:Int, ps:List[Int], v:Int):Int = {
    if (p<ys._1) -1
    else if (ys._1<=p && p <= ys._2) ps.max 
    else positionY(p+v, p+:ps, v-1)
  }
  val input = fromResource("input17.txt").getLines().next
  val cs = input.split(",")
  /** x coordinates of the target region */ 
  val xs = scan(cs(0))
  val vx = distanceX(0,0,xs)
  /** y coordinates of the target region */ 
  val ys = scan(cs(1))
  val maxs = for (y<- 0 to 1000) yield positionY(0,List[Int](), y)
  println(s"Part 1: ${maxs.max}")
  // --------------------------------------------------------------- Part 2
  @tailrec
  def positionXY(px:Int, py: Int, vx:Int, vy:Int):Boolean = {
    if (py<ys._1) false
    else if (ys._1<=py && py <= ys._2 && xs._1<=px && px <= xs._2) true 
    else positionXY(px+vx, py+vy, (vx-1).max(0), vy-1)
  }
  val g = for (y<- -500 to 500; x <- 0 to 500 if positionXY(0:Int, 0: Int, x:Int, y:Int)) yield 1
  println(s"Part 2: ${g.size}")
}