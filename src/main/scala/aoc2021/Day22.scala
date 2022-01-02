package aoc2021
import scala.annotation.tailrec
import scala.io.Source.fromResource
import scala.util.Random

case class Cubic(xl:Int, xh:Int, yl:Int, yh: Int, zl:Int, zh:Int, sign:Int, level:Int) {
  val volume:BigInt = {
    def factor(h:Int, l:Int):BigInt = (h-l) max 0
    val v:BigInt = factor(this.xh,this.xl)*factor(this.yh,this.yl)*factor(this.zh,this.zl)
    v
  }

  /**
    * Create the intersection with another cubic.
    *
    * @param that another cubic (new one)
    * @return the intersection as cubic
    */
  def intersect(that: Cubic): Cubic = {
    /** what is the parity of intersect an old and a new cubic? */
    val c = Map(( 1,1) -> -1,   // Inclusion–exclusion principle
                (-1,1) ->  1,   // Inclusion–exclusion principle
                ( 1,0) -> -1,   // deleting old cubes
                (-1,0) ->  1,   // deleting the absence
                ( 0,1) ->  0,
                ( 0,0)->   0)
    val xl = this.xl max that.xl
    val yl = this.yl max that.yl
    val zl = this.zl max that.zl
    val xh = this.xh min that.xh
    val yh = this.yh min that.yh
    val zh = this.zh min that.zh
    Cubic(xl,xh,yl,yh,zl,zh,c((this.sign,that.sign)),this.level+that.level)
  }

  /**
    * Which pixels are in the cubic?
    *
    * @return Set of pixels
    */
  def innerPixels:Set[(Int,Int,Int)] =
    (for (x <- xl until xh; y <- yl until yh; z <- zl until zh) yield (x,y,z)).toSet
  
  /**
    * For the first part we need to restrict the cubics.
    *
    * @return the given cubic satisfy this property?
    */
  def part1:Boolean = xl.abs<=51 && xh.abs<=51 && yl.abs<=51 && yh.abs<=51 && zl.abs<=51 && zh.abs<=51
  override def toString: String = s"(${this.xl}..${this.xh},${this.yl}..${this.yh},${this.zl}..${this.zh}/${this.sign}:${this.volume}:${this.level})"
}
object Day22 extends App {
  /**
    * Generate a cubic description for testing.
    */
  def generateLine: Unit = {
    val (l,h) = (5,25)
    val x1 = Random.between(l,h)
    val x2 = Random.between(l,h)
    val y1 = Random.between(l,h)
    val y2 = Random.between(l,h)
    val z1 = Random.between(l,h)
    val z2 = Random.between(l,h)
    val nf = if (Random.nextBoolean()) "on" else "off"
    println(s"$nf x=${x1 min x2}..${x1 max x2},y=${y1 min y2}..${y1 max y2},z=${z1 min z2}..${z1 max z2}")
    //on x=10..12,y=10..12,z=10..12
  }
  /**
    * Generate a sequence of cubics
    *
    * @param l number of cubics
    */
  def generateProblem(l:Int):Unit = { for (i <- 1 to l) generateLine }
  
  /** 
    * Parse the coordinate data
    *
    * @param s string contains the min..max pair
    * @return (min,max) pair of integers
    */
  def scan(s:String):(Int,Int) = {
    val e = s.indexOf('=')
    val p = s.indexOf('.')
    val a = s.substring(e+1,p).toInt
    val b = s.substring(p+2).toInt
    (a,b)
  }

  /**
    * parse one line of the input
    * @param s one line of the input
    * @return status and coordinates
    */
  def processALine(s:String): (Boolean, Array[(Int,Int)]) = {
    val cs = s.split(",")
    val corners = cs.map(scan(_))
    val on = s.take(2) == "on"
    (on, corners)
  }

  def createCubic(c:Array[(Int,Int)], s:Boolean) = {
    if (s) Cubic(c(0)._1,c(0)._2+1, c(1)._1,c(1)._2+1,c(2)._1,c(2)._2+1, 1,1) // on 
    else   Cubic(c(0)._1,c(0)._2+1, c(1)._1,c(1)._2+1,c(2)._1,c(2)._2+1, 0,1) // off
  }

  /**
    * Construct the intersection of cubic c and the former cubics
    *
    * @param c new cubic
    * @param l old cubics - even intersections
    * @param acc accumulator for creating the new list
    * @return list of valid intersections
    */
  @tailrec
  def addSections(c:Cubic, l:List[Cubic], acc:List[Cubic]): List[Cubic] = {
    if (l.isEmpty) acc
    else {
      val i = l.head.intersect(c)
      if (i.volume == 0) addSections(c, l.tail, acc)
      else addSections(c, l.tail, i :: acc)
    }
  }
  @tailrec
  def advancedMethod(n:Int, cs:List[Cubic], acc:List[Cubic]):BigInt = {
    if (n<=0 || cs.isEmpty) acc.map(c => c.volume*c.sign).sum
    else {
      val is = addSections(cs.head, acc, List[Cubic]())
      advancedMethod(n-1, cs.tail, (cs.head +: acc) ++ is)
    }
  }
  /**
    * calculate the remaining pixels after step n (works only for toy problems)
    *
    * @param n number of steps
    * @param cs list of cubics
    * @param s accumulator for the pixels
    * @return number of pixels
    */
  @tailrec
  def bruteForce(n:Int, cs:List[Cubic], s:Set[(Int,Int,Int)]):Int = {
    if (n<=0 || cs.isEmpty) s.size
    else {
      val c = cs.head
      val sc = c.innerPixels
      //println(c, s.size, sc.size)
      if (c.sign == 1) bruteForce(n-1, cs.tail, s union sc)
      else bruteForce(n-1, cs.tail, s diff sc)
    }
  }

  // just for testing
  def intersections(n:Int, cs:List[Cubic]) = {
    val ns = (0 to n).toSet
    val ss = ns.subsets()
    for (i <- ss){
      var c = cs(n+1)
      for (j <- i) {c = c.intersect(cs(j))}
      if (c.volume>0) println(s"${n+1}-$i;${c.volume};$c")
    }
  }

  val input = fromResource("input22.txt").getLines().toList
  val cs = for (l <- input) yield {
    val p = processALine(l)
    createCubic(p._2,p._1)
  }
  /* testing the idea
  for (i <- 1 to cs.size){
    val v1 = bruteForce(i, cs, Set[(Int,Int,Int)]())
    val v2 = advancedMethod(i, cs, List[Cubic]())
    if (v1!=v2) println(i,v1,v2)
  }
  println("finish")
  */
  
  val cs1 = cs.filter(_.part1)
  //print(cs1)
  val sol1 = advancedMethod(1000,cs1,List[Cubic]())
  println(s"Part 1: $sol1") 
  //--------------------------------------------------- Part 2
  //val input2 = fromResource("input22ap2.txt").getLines().toList
  val input2 = fromResource("input22.txt").getLines().toList
  val cs2 = for (l <- input2) yield {
    val p = processALine(l)
    createCubic(p._2,p._1)
  }
  //println(cs2)
  val sol2 = advancedMethod(1000,cs2,List[Cubic]())
  println(s"Part 2: $sol2") 
}
