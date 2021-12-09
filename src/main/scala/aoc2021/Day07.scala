package aoc2021

import scala.io.Source.fromResource
object Day07 extends App {
    val items = fromResource("input07.txt").getLines()
    val nums = items.next.split(',').map(_.toInt).toList.sorted
    println(nums)
    def totalDistance(l:Int, ns:List[Int]):Int = ns.map(n => (n-l).abs).sum
    val d = (nums.head to nums.last).map(n => totalDistance(n,nums)).min
    println("Part 1: " + d.toString)
    //Part 2
    def sumNum(n:Int) = n*(n+1)/2
    def totalSumDistance(l:Int, ns:List[Int]):Int = ns.map(n => sumNum((n-l).abs)).sum
    val d2 = (nums.head to nums.last).map(n => totalSumDistance(n,nums)).min
    println("Part 2: " + d2.toString)

}