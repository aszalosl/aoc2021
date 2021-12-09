package aoc2021

import scala.io.Source.fromResource
import scala.annotation.tailrec
object Day08 extends App {
    val items = fromResource("input08.txt").getLines()
    val outputs = items.map(_.substring(61).split(' ').map(_.size)).flatten  
    println("Part 1: " + outputs.filter(x => x==7|x<5).size.toString)

    def process(sc: String,so:String) = {
        val ns:List[Set[Char]] = sc.split(' ').sortBy(_.size).map(_.toSet).toList
        val os:List[Set[Char]] = so.split(' ').map(_.toSet).toList
        val l5 = ns.filter(_.size==5)
        val l6 = ns.filter(_.size==6)
        var hm = new scala.collection.mutable.HashMap[Set[Char],Int] 
        hm addAll List(ns(0)->1, ns(1)->7, ns(2)->4, ns(9)->8)
        val s1 = l6.map(x => l5.filter(_ subsetOf x).size)
        val s2 = l5.map(x => l6.filter(x subsetOf _).size)
        for (i <- 0 to 2){
            if (s1(i)==0) hm += l6(i) -> 0
            if (s1(i)==1) hm += l6(i) -> 6
            if (s1(i)==2) hm += l6(i) -> 9
            if (s2(i)==0) hm += l5(i) -> 2
            if (s2(i)==1) hm += l5(i) -> 3
            if (s2(i)==2) hm += l5(i) -> 5
        }
        //println(hm)
        hm(os(0))*1000+hm(os(1))*100+hm(os(2))*10+hm(os(3))
    }

    val items2 = fromResource("input08.txt").getLines()
    val xs = items2.map(x => process(x.substring(0,59), x.substring(61))).toList.sum
    println(xs)
}

