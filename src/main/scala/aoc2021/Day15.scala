package aoc2021

import scala.annotation.tailrec
import scala.io.Source.fromResource
import scala.collection.mutable.HashMap
object Day15 extends App {
    //TODO: imperative style - need to rewrite later
    def count1(x:Int,y:Int, m:HashMap[(Int,Int),Int]):Int = {
      // println(s"$x - $y")
      val v = m.getOrElse((x,y),tooMuch)
      val a = m.getOrElse((x-1,y),tooMuch)
      val b = m.getOrElse((x,y-1),tooMuch)
      val c = m.getOrElse((x+1,y),tooMuch)
      val d = m.getOrElse((x,y+1),tooMuch)
      val mv = (a min b min c min d) + items(y).charAt(x).toInt-48
      v min mv
    }
    def count2(x:Int,y:Int, m:HashMap[(Int,Int),Int]):Int = {
      // println(s"$x - $y")
      val v = m.getOrElse((x,y),tooMuch*25)
      val a = m.getOrElse((x-1,y),tooMuch*25)
      val b = m.getOrElse((x,y-1),tooMuch*25)
      val c = m.getOrElse((x+1,y),tooMuch*25)
      val d = m.getOrElse((x,y+1),tooMuch*25)
      val mv = (a min b min c min d) + vs((x,y))
      v min mv
    }
    def printTable():Unit = {
        for (y <- 0 until sizeY){
            for (x <- 0 until sizeX){
                print(f" ${m((x,y))}%3d")
            }
            println()
        }
        println()
    }
    def printPuzzle():Unit = {
        for (y <- 0 until sizeY*5){
            for (x <- 0 until sizeX*5){
                print(f" ${vs((x,y))}%1d")
            }
            println()
        }
        println()
    }
    def findPath(m:HashMap[(Int,Int),Int], 
                 sx:Int, sy:Int, limit:Int,
                 cnt: (Int, Int, HashMap[(Int,Int),Int]) => Int):Unit = {
        var changed=true
        do {
        changed = false
        for (y <- 0 until sy; x <- 0 until sx){
            val c = cnt(x,y,m)
            if (m.getOrElse((x,y),limit) != c) {
                changed = true
                m((x,y))=c
            }
        }
        for (y <- sy-1 to 0 by -1; x <- sx-1 to 0 by -1){
            val c = cnt(x,y,m)
            if (m.getOrElse((x,y),limit) != c) {
                changed = true
                m((x,y))=c
            }
        }
        print("+")
        } while (changed)
    }
    val items = fromResource("input15.txt").getLines().toList
    val sizeY=items.size
    val sizeX=items(0).size
    val tooMuch = sizeY*sizeX*10

    var m = new HashMap[(Int,Int), Int] 
    m((0,0))=0
    findPath(m,sizeX,sizeY, tooMuch, count1)
    val sol1 = m((sizeX-1,sizeY-1)) 
    println(s"Part 1: $sol1")
    /* ------------------------------------------------- Part 2 */
    var vs = new HashMap[(Int,Int), Int]
    for (y <- 0 until sizeY; x <- 0 until sizeX){
      val n = items(y).charAt(x)-48-1
      for (u <- 0 until 5; v <- 0 until 5){
        vs((x+sizeX*u,y+sizeY*v)) = (n + u + v) % 9 + 1 
      }
    }
    var m2 = new HashMap[(Int,Int), Int] 
    m2((0,0))=0
    findPath(m2, sizeX*5, sizeY*5, tooMuch*25, count2)
    val sol2 = m2((sizeX*5-1,sizeY*5-1)) 
    println(s"Part 2: $sol2")
}