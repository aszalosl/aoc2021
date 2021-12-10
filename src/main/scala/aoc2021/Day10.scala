package aoc2021

import scala.io.Source.fromResource
import scala.annotation.tailrec
object Day10 extends App {
    private val pair = Map('('-> ')', '{'-> '}', '['-> ']', '<'-> '>' )
    private val score = Map(')'-> 3, ']'-> 57, '}'-> 1197, '>'-> 25137)
    @tailrec
    def checkline(line:String, stack:String):Int = {
        if (line.isEmpty) 0 
        else if (pair contains line.head) checkline(line.tail, (pair apply line.head) + stack)
        else if (stack.nonEmpty && line.head==stack.head) checkline(line.tail, stack.tail)
        else score apply line.head
    }
    val items = fromResource("input10.txt").getLines.map(checkline(_,"")).toList
    println(s"Part 1: ${items.sum}" )

    private val score2 = Map(')'-> 1, ']'-> 2, '}'-> 3, '>'-> 4)
    @tailrec
    def checkline2(line:String, stack:String):String = {
        if (line.isEmpty) stack
        else if (pair contains line.head) checkline2(line.tail, (pair apply line.head) + stack)
        else if (stack.nonEmpty && line.head==stack.head) checkline2(line.tail, stack.tail)
        else ""
    }
    @tailrec
    def point(ends:String, p:BigInt):BigInt = {
        if (ends.isEmpty) p
        else point(ends.tail,p*5 + (score2 apply ends.head))
    }
    val items2 = fromResource("input10.txt").getLines.map(checkline2(_,"")).filter(_.nonEmpty).map(point(_,0L)).toList
    val o2 = items2.sorted
    println(o2(o2.size/2))
    

}