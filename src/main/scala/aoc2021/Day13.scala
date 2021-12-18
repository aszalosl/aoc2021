package aoc2021
import scala.annotation.tailrec
import scala.io.Source.fromResource
object Day13 extends App {
  
  def mirrorX(x:Int, ps:List[(Int,Int)]) = ps.map(p => (x-(p._1-x).abs,p._2)).toSet.toList
  def mirrorY(y:Int, ps:List[(Int,Int)]) = ps.map(p => (p._1,y-(p._2-y).abs)).toSet.toList
  @tailrec
  def mirrors(ms:List[(Char,Int)],ps:List[(Int,Int)]):List[(Int,Int)] = {
    if (ms.isEmpty) ps
    else if (ms.head._1 == 'x') mirrors(ms.tail, mirrorX(ms.head._2,ps))
    else mirrors(ms.tail, mirrorY(ms.head._2,ps))
  }
  def printBorder(ps:List[(Int,Int)]) {
    val xMin=ps.map(_._1).min
    val xMax=ps.map(_._1).max
    val yMin=ps.map(_._2).min
    val yMax=ps.map(_._2).max
    println(s" $xMin,$yMin - $xMax,$yMax")
  }
  val items = fromResource("input13.txt").getLines().toList
  val nums = items.filter(_ contains ',').map(_.split(',')).map(xy => (xy(0).toInt,xy(1).toInt)).toList
  val mirrs = items.filter(_ contains '=').map(s => (s.charAt(s.indexOf('=')-1), s.substring(s.indexOf('=')+1).toInt))
  println(mirrs)
  
  val p1 = mirrors(List(mirrs(0)), nums)
  println(s"Part1 : ${p1.size}")
  val p2 = mirrors(mirrs, nums)
  val xMax=p2.map(_._1).max
  val yMax=p2.map(_._2).max
  for (y <- 0 to yMax) {
    for (x <- 0 to xMax)
      if (p2 contains (x,y)) print("*") else print(" ")
    println()
  }
  
}