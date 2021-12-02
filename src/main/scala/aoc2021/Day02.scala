package aoc2021

import scala.io.Source.fromResource
object Day02 extends App {
    def direction(dir:String, length:Int) =
        dir match {
            case "up" => (0,-length)
            case "down" => (0,length)
            case "forward" => (length,0)
        } 
    val items = fromResource("input02.txt")
        .getLines()
        .map(x => x.split(' '))
        .map(x => (x(0), x(1).toInt)).toList
        .map(x => direction(x._1, x._2))
    //first solution
    //val (xs,ys) = items.unzip
    //println("Part 1: " + (xs.sum * ys.sum).toString())
    def doubleSum(ps:List[(Int,Int)]) = ps.foldLeft((0,0)){
        case ((dx,dy),p) => (dx+p._1, dy+p._2)
    }
    val (x,y) = doubleSum(items)
    println("Part 1: " + (x * y).toString)
    
    def secondMethod(ps:List[(Int,Int)]) = ps.foldLeft((0,0,0)){
        case ((dx,dy,aim), p) => 
            if (p._1 == 0) (dx,dy,aim+p._2) else (dx+p._1, dy+p._1*aim,aim) 
    }

    val z = secondMethod(items)
    println("Part 2: " + (z._1 * z._2).toString)

}