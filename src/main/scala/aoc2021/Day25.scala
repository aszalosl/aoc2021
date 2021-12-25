package aoc2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day25 extends App {
  def below(xy:(Int,Int)) = {
    val x = xy._1
    val y = (xy._2 + 1) % sizeY
    (x,y)
  }
  def right(xy:(Int,Int)) = {
      val x = (xy._1 + 1) % sizeX 
      val y = xy._2
    (x,y)
  }
  def goDown(d:Set[(Int,Int)], e:Set[(Int,Int)]):Set[(Int,Int)] = {
      val d1 = for (xy <- d if (!d(below(xy)) && !e(below(xy)))) yield below(xy)
      val d2 = for (xy <- d if (d(below(xy)) || e(below(xy)))) yield xy
      (d1++d2).toSet
  }
  def goRight(d:Set[(Int,Int)], e:Set[(Int,Int)]):Set[(Int,Int)] = {
      val e1 = for (xy <- e if (!d(right(xy)) && !e(right(xy)))) yield right(xy)
      val e2 = for (xy <- e if (d(right(xy)) || e(right(xy)))) yield xy
      //print(s"move to: $e1\nstay: $e2\n")
      (e1++e2).toSet
  }
  @tailrec
  def oneStep(d:Set[(Int,Int)], e:Set[(Int,Int)], n:Int):Int = {
    val e1 = goRight(d,e)
    val d1 = goDown(d,e1)
    //if (n>10) n
    if (e1.equals(e) && d1.equals(d)) n
    else {
      //printTable(d,e,n)
      oneStep(d1,e1,n+1)
    }
  }
  def printTable(d:Set[(Int,Int)],e:Set[(Int,Int)], n:Int) {
    println(n)
    for (y <- 0 until sizeY) {
      for (x <- 0 until sizeX) {
        if (d((x,y))) print('v')
        else if (e((x,y))) print('>')
        else print('.')
      }
      println()
    }
    println
  }
  val items = fromResource("input25.txt").getLines().toList
  val sizeX = items(0).size
  val sizeY = items.size
  
  val down = (for (x <- 0 until sizeX; y <- 0 until sizeY if items(y).charAt(x)== 'v') yield (x,y)).toSet
  val east = (for (x <- 0 until sizeX; y <- 0 until sizeY if items(y).charAt(x)== '>') yield (x,y)).toSet
  val p1 = oneStep(down, east,0)+1
  println(s"Part 1: $p1")
}