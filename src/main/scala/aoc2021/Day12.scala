package aoc2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day12 extends App {
  /** the input of the puzzle as list of pair of words */
  val input = fromResource("input12.txt").getLines.map(_.split('-')).toList
  /** names of the rooms */
  val names = input.flatten.toSet.toList
  /** words as numbers */
  val lookup = names.zipWithIndex.toMap
  /** big caves */
  val repeats = names.filter(x => x == x.toUpperCase).map(lookup(_))
  /** arcs between nodes */
  val graph = input.map(x => (lookup(x(0)), lookup(x(1))))
  val backwards = graph.map(x => (x._2,x._1))
  val nextNode = (graph ++ backwards).groupBy(_._1).map(x =>(x._1,x._2.map(_._2)))
  val start = lookup("start")
  val stop = lookup("end")
  /**
    * Find all the path from start to end, following the rules of the puzzle
    *
    * @param ps list of active paths
    * @param c number of full paths found
    */
  def dfs(ps: List[List[Int]], c:Int):Int ={
      if (ps.isEmpty) c
      else {
          val currentNode = ps.head(0)
          if (stop == currentNode) dfs(ps.tail, c+1)
          else {
            val newPaths = nextNode(currentNode).map(x => x +: ps.head).filter(p => (repeats contains p.head) || !(p.tail contains p.head))
            dfs(newPaths ++ ps.tail, c)
          }
      }
  }
  val sol1 = dfs(List(List(start)),0)
  println(s"Part 1: $sol1")
  /**
    * x occurs maximum once in list xs?
    *
    * @param x element to search
    * @param xs list of previous caves 
    * @param yet we have found one occurence yet
    * @return at most one occurence of x in xs
    */
  @tailrec
  def maxOneDouble(xs:List[Int], ys:Set[Int], yet: Boolean):Boolean = {
      if (xs.isEmpty) true
      else if (repeats contains xs.head) maxOneDouble(xs.tail, ys, yet)
      else if (ys.contains(xs.head) && yet) false
      else if (ys.contains(xs.head)) maxOneDouble(xs.tail, ys, true)
      else maxOneDouble(xs.tail, ys+xs.head, yet)
  }
   @tailrec
   def dfs2(ps: List[List[Int]], c:Int):Int ={
      if (ps.isEmpty) c
      else {
          val currentNode = ps.head(0)
          if (stop == currentNode) {
            //println(ps.head.reverse)
            dfs2(ps.tail, c+1)
          }
          else {
            val newPaths = nextNode(currentNode)
            .map(x => x +: ps.head)
            .filter(p => (p.head != start && maxOneDouble(p,Set[Int](),false)))
            dfs2(newPaths ++ ps.tail, c)
          }
      }
  }
  val sol2 = dfs2(List(List(start)),0)
  println(s"Part 2: $sol2")
  
}