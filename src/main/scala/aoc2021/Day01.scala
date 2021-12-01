package aoc2021

import scala.io.Source.fromResource
object Day01 extends App {
    val items: List[Int] = fromResource("input01.txt").getLines().map(x => x.toInt).toList
    def increments(ls: List[Int]) = (ls zip ls.tail).filter(x => x._1<x._2).size
    
    println("Part 1: " + increments(items).toString())
    println("Part 2: " + increments((items, items.tail, items.tail.tail).zipped.toList.map(x => x._1+x._2+x._3)))
}