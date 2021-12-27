package aoc2021

import scala.annotation.tailrec
import scala.io.Source.fromResource
object Day20 extends App {
  /** the picture as the set of pixels */
  type Table = Set[(Int,Int)]
  /**
    * Due the handling of code 0, we need to separate pixels outside the picture based on parity of the step. 
    *
    * @param d  the picture 
    * @param x  actual x-coordinate
    * @param y  actual y-coordinate
    * @param xl left corner of the picture
    * @param xh right corner of the picture
    * @param yl upper corner of the picture
    * @param yh lower corner of the picture
    * @param p  parity of enhancement
    * @return the pixel is light one?
    */
  def lookup(d:Table,x:Int,y:Int, xl:Int,xh:Int,yl:Int,yh:Int, p:Int) =
    if (p%2==1) {
      if (x<xl || x>xh || y<yl || y>yh) false else d contains (x,y)
    } else {
      if (x<xl || x>xh || y<yl || y>yh) true else d contains (x,y)
    }
  /**
    * the index of the pixel based its neighbourhood
    *
    * @param x actual x-coordinate
    * @param y actual y-coordinate
    * @param d picture
    * @param xl left corner
    * @param xh right corner
    * @param yl upper corner
    * @param yh lower corner
    * @param n parity of enhacement
    * @return the index in the lookup-table (first line of the input)
    */
  def code(x:Int, y:Int,d:Table,  xl:Int,xh:Int,yl:Int,yh:Int, n:Int):Int = {
    val b9 = if (lookup(d,x-1,y-1,xl,xh,yl,yh,n))   256 else 0
    val b8 = if (lookup(d,x,  y-1,xl,xh,yl,yh,n))   128 else 0
    val b7 = if (lookup(d,x+1,y-1,xl,xh,yl,yh,n))    64 else 0
    val b6 = if (lookup(d,x-1,  y,xl,xh,yl,yh,n))    32 else 0
    val b5 = if (lookup(d,x,    y,xl,xh,yl,yh,n))    16 else 0
    val b4 = if (lookup(d,x+1,  y,xl,xh,yl,yh,n))     8 else 0
    val b3 = if (lookup(d,x-1,y+1,xl,xh,yl,yh,n))     4 else 0
    val b2 = if (lookup(d,x,  y+1,xl,xh,yl,yh,n))     2 else 0
    val b1 = if (lookup(d,x+1,y+1,xl,xh,yl,yh,n))     1 else 0
    b1+b2+b3+b4+b5+b6+b7+b8+b9
  }
  /**
    * This value occurs in the lookup table?
    *
    * @param c code of the actual pixel - based on it neighbourhood
    * @param s lookup-table as a set
    * @return c is in the lookup table?
    */
  def survive(c:Int, s:Set[Int]) = s(c)
  /**
    * What are the limits of the picture?
    *
    * @param d picture
    * @return left, right, lower and upper corners
    */
  def borders(d:Table) = {
    val xs = for (xy <- d) yield xy._1 
    val ys = for (xy <- d) yield xy._2
    (xs.min, xs.max, ys.min, ys.max) 
  }
  /**
    * calculate the next picture
    *
    * @param d actual picture
    * @param s lookup table
    * @param n parity of the enhancement
    * @return the following picture 
    */
  def nextState(d:Table, s:Set[Int], n:Int) = {
    val (xl,xh,yl,yh) = borders(d)
    val dNew = for ( x <- xl-2 to xh+2; y <- yl-2 to yh+2; if survive(code(x,y,d,xl,xh,yl,yh,n), s) ) yield (x,y)
    dNew
  }
  /**
    * Just for testing - print the picture
    *
    * @param d picture
    */
  def printTable(d:Table) = {
    val (xl,xh,yl,yh) = borders(d)
    for (y <- yl-1 to yh+1) {
      for (x <- xl-1 to xh+1 ) {
        if (d contains (x,y)) print('#') else print('.')
      }
      println()
    }
    println()
  }
  /** input as list of strings */
  val items = fromResource("input20.txt").getLines().toList
  
  val s0 = for (i <- 0 until 512 if items(0).charAt(i)=='#') yield i
  /** the lookup table */
  val s = s0.toSet
  val d0 = for (j <- 2 until items.size; i <- 0 until items(2).size if items(j).charAt(i)=='#') yield (i,j)
  /** original picture */
  val d = d0.toSet
  /** first follower */
  val d1 = nextState(d,s,1).toSet
  /** second follower */
  val d2 = nextState(d1,s,2).toSet
  println(s"Part 1: ${d2.size}")
  //---------------------------------------------------------- Part 2
  /**
    * Execute the series of enhancement
    *
    * @param n number of enhancements
    * @param d picture
    * @param s lookup table
    * @return picture after n enhancements
    */
  def enhance(n:Int, d:Table, s:Set[Int]): Table = {
    if (n==0) d
    else {
      val d1 = nextState(d,s,n+1).toSet
      enhance(n-1,d1,s)
    }
  }
  val d3 = enhance(50,d,s)
  /** final picture */
  printTable(d3)
  println(s"Part 2: ${d3.size}")
  
}