package aoc2021
import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day14 extends App {
  val items = fromResource("input14.txt").getLines().toList
  val rules = items.filter(_ contains '>').map(l => (l.substring(0,2),l.last))
  val result1 = rules.map(l => (l._1, l._1.charAt(0).toString ++ l._2.toString))
  var result2 = rules.map(l => (l._1, l._2.toString ++ l._1.charAt(1).toString))
  var transform = result1++result2
  /**
    * Calculate the pairs in the string after one step
    *
    * @param tr rewriting rules
    * @param m actual count of pair-occurences
    * @return new count of pair occurences
    */ 
  def oneStep(tr:List[(String,String)], m:Map[String,Long]): Map[String,Long] = {
      // all rules applied then summed by pairs
      val x = tr.map(st => (st._2, m.getOrElse(st._1, 0L)))
                .groupBy(_._1).mapValues(_.map(_._2).sum).toMap
      //println(s"1step: $x")
      //println(x.values.sum, countChars(x), x.filter{case (k,v) => v!=0})
      x
   }
  @tailrec
  /**
    * Execute several steps
    *
    * @param s number of remaining steps
    * @param tr rewriting rules
    * @param m actual count of pair-occurences
    * @return new count of pair occurences
    */
  def manySteps(s:Int, tr:List[(String,String)], m:Map[String,Long]): Map[String,Long] = {
      if (s<=0) m
      else {
        val m1 = oneStep(tr, m)
        manySteps(s-1, tr, m1)
      }
    }
  /**
    * calculate the numbers of the chars
    *
    * @param m actual count of pair-occurences
    * @return number of occurences of chars (except the last one)
    */
  def countChars(m:Map[String,Long]): Map[Char,Long] = {
      val m1 = m.toList.map{z => (z._1.charAt(0),z._2)}
      m1.groupBy(_._1).mapValues(_.map(_._2).sum).toMap
    }
  val lastChar = items(0).last
  val vs = items(0).sliding(2).map(p => (p,1L)).toList.groupBy(_._1).mapValues(_.map(_._2).sum).toMap
  val v2 = manySteps(10,transform,vs)
  val cm = countChars(v2).map{case (k,v) => if (k==lastChar) (k,v+1) else (k,v)}
  val sol1 = cm.values.max-cm.values.min
  println(s"Part 1: $sol1")
  val v3 = manySteps(40,transform,vs)
  val cm2 = countChars(v3).map{case (k,v) => if (k==lastChar) (k,v+1) else (k,v)}
  println(cm2)
  val sol2 = cm2.values.max-cm2.values.min
  println(s"Part 2: $sol2")
}