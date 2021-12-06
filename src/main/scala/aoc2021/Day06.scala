package aoc2021

import scala.io.Source.fromResource
import scala.annotation.tailrec
object Day06 extends App {
    val items = fromResource("input06.txt").getLines()
    val nums = items.next.split(',').map(_.toInt).groupBy(identity).mapValues(_.size)
    val lf0 = nums.getOrElse(0,0)
    val lf1 = nums.getOrElse(1,0)
    val lf2 = nums.getOrElse(2,0)
    val lf3 = nums.getOrElse(3,0)
    val lf4 = nums.getOrElse(4,0)
    val lf5 = nums.getOrElse(5,0)
    val lf6 = nums.getOrElse(6,0)
    val lf7 = 0
    val lf8 = 0

    @tailrec
    def nextFish(l0:BigInt,l1:BigInt,l2:BigInt,l3:BigInt,l4:BigInt,l5:BigInt,l6:BigInt,l7:BigInt,l8:BigInt,c:Int): BigInt = {
        //println(c)
        if (c<=0) l0+l1+l2+l3+l4+l5+l6+l7+l8
        else nextFish(l1,l2,l3,l4,l5,l6,l7+l0,l8,l0,c-1)
    }
    println("Part 1: " + nextFish(lf0,lf1,lf2,lf3,lf4,lf5,lf6,lf7,lf8,80).toString)
    println("Part 2: " + nextFish(lf0,lf1,lf2,lf3,lf4,lf5,lf6,lf7,lf8,256).toString)

}