package aoc2021
import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable
import scala.io.Source.fromResource

object Day19 extends App {
  type Matrix = IndexedSeq[IndexedSeq[Int]] // to denote a rotation matrix
  type Point = (Int, Int, Int) //a 3D point
  type Vector3D = (Int, Int, Int) // 3D vector

  /**
    * Combine two rotations
    *
    * @param a first rotation matrix
    * @param b second rotation matrix
    * @return their composition (matrix product)
    */
  def matrixProduct(a: Matrix, b: Matrix): Matrix = {
    val m = for (i <- 0 to 2) yield {
      for (j <- 0 to 2) yield
        a(i)(0) * b(0)(j) + a(i)(1) * b(1)(j) + a(i)(2) * b(2)(j)
    }
    m
  }

  /**
    * Find all the rotations in 3D around axes
    */
  def calculateRotations(): Unit = {
    val a = IndexedSeq(IndexedSeq(0, -1,  0), IndexedSeq(1, 0,  0), IndexedSeq(0, 0, 1)) //xy
    val b = IndexedSeq(IndexedSeq(0,  0, -1), IndexedSeq(0, 1,  0), IndexedSeq(1, 0, 0)) //xz
    val c = IndexedSeq(IndexedSeq(1,  0,  0), IndexedSeq(0, 0, -1), IndexedSeq(0, 1, 0)) //yz
    var s: mutable.Set[Matrix] = mutable.Set(a, b, c)
    var continue = true
    do {
      val newMatrix = for (x <- s; y <- s; z = matrixProduct(x, y); if !(s contains z)) yield z
      s ++= newMatrix.toSet
      if (newMatrix.isEmpty) continue = false
    } while (continue)
    for (x <- s) println(x)
  }

  /**
    * Rotate a point in 3D
    *
    * @param point a 3D point in the space
    * @param id    id of the rotation
    * @return the point after the rotation
    */
  def rotate(point: Point, id: Int): Point = point match {
    case (x, y, z) => id match {
      case 0 => (x, y, z)
      case 1 => (-y, -x, -z)
      case 2 => (y, -z, -x)
      case 3 => (-x, -y, z)
      case 4 => (y, z, x)
      case 5 => (z, y, -x)
      case 6 => (-x, y, -z)
      case 7 => (y, x, -z)
      case 8 => (-z, -x, y)
      case 9 => (z, -y, x)
      case 10 => (x, -y, -z)
      case 11 => (-z, x, -y)
      case 12 => (-x, z, y)
      case 13 => (-z, y, x)
      case 14 => (-y, x, z)
      case 15 => (z, -x, -y)
      case 16 => (-x, -z, -y)
      case 17 => (y, -x, z)
      case 18 => (x, -z, y)
      case 19 => (-y, z, -x)
      case 20 => (-y, -z, x)
      case 21 => (-z, -y, -x)
      case 22 => (z, x, y)
      case 23 => (x, z, -y)
    }
  }

  /**
    * Which rotation turns a to b?
    *
    * @param a vector to rotate
    * @param b goal vector
    * @return index of rotation
    */
  def rotated(a: Vector3D, b: Vector3D): Option[Int] = {
    val indices = for {i <- 0 to 23 if b == rotate(a,i)} yield i
    if (indices.isEmpty) None
    else Some(indices.head)
  }

  /**
    * Try to combine two set of points
    * @param xs original points
    * @param ys points to rotate
    * @param i  index of focus point in the first set
    * @param j  index of focus point in the second set
    * @param r  index of the rotation
    * @return if there are at least 12 common point, then the union, otherwise empty set
    */
  def similarity(xs: List[Point], ys: List[Point], i: Int, j: Int, r: Int): (Vector3D, List[Point]) = {
    /**
      * Shift a point with a vector
      * @param z 3D point
      * @param d 3D vector
      * @return shifted point
      */
    def move(z: Point, d: Vector3D): Point = (z._1 + d._1, z._2 + d._2, z._3 + d._3)
    val x = xs(i)
    val y = rotate(ys(j), r)
    val d: Vector3D = (x._1 - y._1, x._2 - y._2, x._3 - y._3)
    val rotatedYs = ys.map(p => rotate(p, r)).map(q => move(q, d))
    if ((xs.toSet intersect rotatedYs.toSet).size < 12) ((0,0,0), List[Point]())
    else (d, (xs.toSet union rotatedYs.toSet).toList)
  }

  /**
    * Read the lines of input file into tuples and break into pieces according to markers
    *
    * @param ls input file as a list of strings
    * @param ll output list of list of tuples in progress
    * @return processed input - list of list of tuples
    */
  @tailrec
  def processInput(ls: List[String], ll: List[List[Point]]): List[List[Point]] = {
    if (ls.isEmpty) ll
    else if (ls.head.contains(',')) {
      val l = ls.head.split(',')
      val p = (l(0).toInt, l(1).toInt, l(2).toInt)
      processInput(ls.tail, (p +: ll.head) +: ll.tail)
    } else if (ls.head == "") processInput(ls.tail, List[Point]() +: ll)
    else processInput(ls.tail, ll)
  }

  def myHash(v: Vector3D): Int = (v._1 * v._2 * v._3).abs

  def closestNeighbours(l: List[Point]): List[Vector3D] = {
    def distanceVector(p: Point, q: Point): Vector3D = (p._1 - q._1, p._2 - q._2, p._3 - q._3)

    def vectorLength(p: Vector3D): Int = p._1 * p._1 + p._2 * p._2 + p._3 * p._3

    @tailrec
    def smallestDistance(p: Point, l: List[Point], v: Vector3D = (0, 0, 0), vl: Int = 10000000): Vector3D = {
      if (l.isEmpty) v
      else if (p == l.head) smallestDistance(p, l.tail, v, vl)
      else {
        val r = distanceVector(p, l.head)
        val rl = vectorLength(r)
        if (rl < vl) smallestDistance(p, l.tail, r, rl) else smallestDistance(p, l.tail, v, vl)
      }
    }

    l.map(smallestDistance(_, l))
  }

  def examine(l1: List[Point], l2: List[Point]): (Vector3D, List[Point]) = {
    @tailrec
    def inner(l: IndexedSeq[(Int, Int, Int)]): (Vector3D, List[Point]) =
      if (l.isEmpty) ((0,0,0), List[Point]()) // this could not happen
      else {
        val (i, j, r) = l.head
        val (d,s) = similarity(l1, l2, i, j, r)
        if (s.isEmpty) inner(l.tail) else (d,s)
      }

    val c1 = closestNeighbours(l1)
    val c2 = closestNeighbours(l2)
    val candidates = for (i <- l1.indices; x = l1(i);
                          j <- l2.indices; y = l2(j)
                          if myHash(c1(i)) == myHash(c2(j));
                          r <- rotated(c2(j), c1(i))) yield (i, j, r)
    val (d,s) = inner(candidates)
    (d, s)
  }

  @tailrec
  def outer(ps : List[List[Point]], bacons: List[Vector3D]): (Int,Int) = {
    @tailrec
    def combine(h: List[Point], t: List[List[Point]], acc: List[List[Point]], bacons:List[Vector3D]):
      (List[List[Point]], List[Vector3D]) =
      if (t.isEmpty) (h +: acc, bacons)
      else {
        val (d,s) = examine(h, t.head)
        if (s.isEmpty) combine(h, t.tail, t.head +: acc, bacons)
        else combine(s, t.tail, acc, d +: bacons)
      }

    def maxDistance(b:List[Vector3D]):Int = {
      val ds = for (v1 <- b;
                    v2 <- b) yield (v1._1-v2._1).abs+(v1._2-v2._2).abs+(v1._3-v2._3).abs // Manhattan distance
      ds.max
    }

    if (ps.size == 1) (ps.head.size, maxDistance(bacons))
    else {
      val (c,b) = combine(ps.head, ps.tail, List[List[Point]](), bacons: List[Vector3D])
      outer(c,b)
      }
    }
  val items=fromResource("input19.txt").getLines().toList
  val el=List[Point]()
  val ps = processInput(items,List(el))
  val (c, bs) = outer(ps, List[Vector3D]((0,0,0)))
  println(c,bs)

}
