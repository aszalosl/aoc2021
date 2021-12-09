package aoc2021

import scala.io.Source.fromResource
//import scala.annotation.tailrec
object Day09 extends App {
    val items = fromResource("input09.txt").getLines().map(_.map(_.toInt-48).toVector).toVector
    //println(items)
    val Big=10
    val sizeY=items.size
    val sizeX=items(0).size
    println("X: " + sizeX.toString)
    println("Y: " + sizeY.toString)
    def lower(x:Int, y:Int): Boolean = {
        if (y>0 && items(y)(x)>=items(y-1)(x)) false
        else if (y<sizeY-1 && items(y)(x)>=items(y+1)(x)) false
        else if (x>0 && items(y)(x)>=items(y)(x-1)) false
        else if (x<sizeX-1 && items(y)(x)>=items(y)(x+1)) false
        else true
    }
    val ps = for {  y <- 0 until sizeY;
                    x <- 0 until sizeX
                    if lower(x,y)
                } yield items(y)(x)+1
    println("Part 1: " + ps.sum.toString)

    def min4(a:Int,b:Int,c:Int,d:Int) = Math.min(Math.min(a,b),Math.min(c,d))

    def down(x:Int,y:Int): Tuple2[Int,Int] = {
        val n0 = items(y)(x)
        val n1 = if (x>0) items(y)(x-1) else Big
        val n2 = if (x<sizeX-1) items(y)(x+1) else Big
        val n3 = if (y>0) items(y-1)(x) else Big
        val n4 = if (y<sizeY-1) items(y+1)(x) else Big
        if (n0 < min4(n1,n2,n3,n4)) (x,y)
        else if (n1 <= min4(n0,n2,n3,n4)) down(x-1,y)
        else if (n2 <= min4(n1,n0,n3,n4)) down(x+1,y)
        else if (n3 <= min4(n1,n2,n0,n4)) down(x,y-1)
        else                              down(x,y+1)
        
    }
    val qs = for {  y <- 0 until sizeY;
                    x <- 0 until sizeX
                    if items(y)(x) !=9
                } yield down(x,y)
    val rs = qs.groupBy(identity).mapValues(_.length).map(_._2).toList
    val ts = rs.sorted(Ordering.Int.reverse)
    println("Part 2: " + (ts(0)*ts(1)*ts(2)).toString)

}
