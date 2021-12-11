package aoc2021

import java.util.ArrayDeque
import scala.annotation.tailrec
import scala.io.Source.fromResource
import scala.collection.MapView
object Day11 extends App {
  type Table = Seq[(Int, Int, Int)]
  type Place = (Int, Int)
  val input = fromResource("input11.txt").getLines().toList
  val sizeX = input(0).size
  val sizeY = input.size
  
  /** Represent the table as a  list of triplets (x,y,v)
   * @param ls rows of the table
   * @param x size of the table
   * @param y size of the table
   * @return coordinates and values as triplets
   */
  def makeTable(ls: List[String], x: Int, y: Int): Table = for {
    i <- 0 until y
    j <- 0 until x
  } yield (i, j, ls(i).charAt(j).toInt - 48)

  /** Reverse the makeTable
   * @param l table as triplets
   * @param x size of the table
   * @param y size of the table
   * @return the rows of the table
   */
  def tableToString(l: Table, x: Int, y: Int): List[String] = {
    val tm = l.foldLeft(Map.empty[Place, Char]) { case (map, (x, y, v)) =>
      map + ((x, y) -> (v + 48).toChar)
    }
    val ls =
      for (j <- 0 until y) yield ((for (i <- 0 until x) yield tm((i, j))) )
    ls.toList.map(_.mkString)
  }

  /** Increase the values of the table
   * @param l original table
   * @param acc accumulator variable
   * @return increased table
   */
  @tailrec
  def incEnergy(l: Table, acc: Table): Table =
    if (l.isEmpty) acc
    else incEnergy(l.tail, (l.head._1, l.head._2, l.head._3 + 1) +: acc)

  /** The coordinates of the eight (or less) neighbours
   * @param x x-coordinate
   * @param y y-coordinate
   * @return list of coordinate-pairs
   */
  def neighbourOf(x: Int, y: Int) = {
    val candidate = List(
      (x - 1, y - 1),
      (x - 1, y),
      (x - 1, y + 1),
      (x, y - 1),
      (x, y + 1),
      (x + 1, y - 1),
      (x + 1, y),
      (x + 1, y + 1)
    )
    candidate.filter(xy =>
      0 <= xy._1 && 0 <= xy._2 && xy._1 < sizeX && xy._2 < sizeY
    )
  }
  /** The neighbours of flash places
   * @param l table
   * @return list of coordinates
   */
  def neighbours(l: Seq[Place]): Seq[Place] = l.map(xy => neighbourOf(xy._1, xy._2)).flatten
  
  /** Change the values on the table accorging the flashes
   * @param l table
   * @param ps places of flash
   * @param ns neighbours of flash
   * @param acc accumulator for the table
   * @returns the new table
   */
  @tailrec
  def upgradeTable(l: Table, ps: Set[Place], ns: Map[Place, Int], acc: Table): Table =
    if (l.isEmpty) acc
    else {
      val x = l.head._1
      val y = l.head._2
      val v = l.head._3
      if (ps contains (x,y)) upgradeTable(l.tail, ps, ns, (x, y, 0) +: acc)
      else upgradeTable(l.tail, ps, ns, (x, y, v + ns.getOrElse((x, y), 0)) +: acc)
    }

  /** Put zero to the places of flash
   * @param l table
   * @param fs places of flash
   * @param acc accumulator for the table
   * @returns the new table
   */
  @tailrec
  def cleanTable(l: Table, fs: Set[Place], acc: Table): Table =
    if (l.isEmpty) acc
    else {
      val x = l.head._1
      val y = l.head._2
      val v = l.head._3
      if (fs contains (x,y)) cleanTable(l.tail, fs, (x, y, 0) +: acc)
      else cleanTable(l.tail, fs, (x, y, v) +: acc)
    }

  @tailrec
  def flash(l:Table,ps:Seq[Place]): (Table,Int) = {
    if (l.filter(_._3 > 9).isEmpty){
      val l1 = cleanTable(l,ps.toSet,List())
      (l1, ps.size)
    }
    else {
      val places = l.filter(_._3 > 9).map(xyv => (xyv._1,xyv._2))
      val ns = neighbours(places).groupBy(identity).map(x => x._1->x._2.size)
      val l1 = upgradeTable(l, places.toSet, ns, List())
      flash(l1, ps++places)
    }
  } 
  def oneStep(l: Table): (Table, Int) = flash(incEnergy(l, List()),List())
 
  /**
    * Solve the first part of the problem.
    *
    * @param l     table
    * @param steps remaining steps
    * @param fs    number of flashes
    * @return number of flashes
    */
  def manySteps(l: Table, steps:Int, fs:Int): (Table,Int) = {
    if (steps <= 0) (l,fs)
    else {
      val ti = oneStep(l)
      manySteps(ti._1, steps-1,fs+ti._2)
    }
  }

  /**
    * Solve the second part of the problem.
    *
    * @param l table
    * @param steps actual step
    * @return when flash all octopus first?
    */
  def synchFlash(l: Table, steps:Int): Int = {
      val ti = oneStep(l)
      if (ti._2 == 100) steps
      else synchFlash(ti._1, steps+1)
  
  }
  val items = makeTable(input, sizeY, sizeX)
  val s = manySteps(items,100,0)
  println(s"Part 1:  ${s._2}")
  println(s"Part 2: ${synchFlash(items,1)}")
}
