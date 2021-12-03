package aoc2021
import scala.io.Source.fromResource

object Day03 extends App {
    private def bitCount(i:Int, ls:List[String]) = ls.map(s => s.charAt(i)).filter(_=='1').size
    def gammaEpsilon(ls: List[String]) = {
        val half = ls.size / 2
        val len = ls.head.length
        val gamma = (for (i <- 1 until len if bitCount(i-1, ls)> half) yield len-i).map(scala.math.pow(2,_)).sum.toInt
        val epsilon = scala.math.pow(2,len).toInt-1-gamma
        gamma * epsilon

    }
    val items = fromResource("input03.txt").getLines().toList
    println("Part 1: " + gammaEpsilon(items).toString)
    def oneStepO2(i:Int, ls:List[String]) = 
        if (bitCount(i,ls) >= ls.size/2.0) ls.filter(_.charAt(i)=='1') else ls.filter(_.charAt(i)=='0')
    def oneStepCO2(i:Int, ls:List[String]) = 
        if (bitCount(i,ls) < ls.size/2.0) ls.filter(_.charAt(i)=='1') else ls.filter(_.charAt(i)=='0')
    def generatorRating(i:Int, ls:List[String], f:(Int,List[String]) => List[String]):String = {
        val one:List[String] = f(i,ls)
        if (one.size == 1) one(0) else generatorRating(i+1,one,f)
    }
    val o2 = generatorRating(0,items, oneStepO2)
    val co2 = generatorRating(0,items, oneStepCO2)
    println("Part 2: " + (Integer.parseInt(o2,2)*Integer.parseInt(co2,2)).toString)
}
