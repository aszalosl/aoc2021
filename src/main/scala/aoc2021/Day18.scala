package aoc2021

import scala.annotation.tailrec
import scala.io.Source.fromResource
object Day18 extends App {
  type Tree = List[Tuple2[String, Int]]
  def parseTree(s:String): Tree = parseTree(s,"",List[(String,Int)]())
  /**
    * create a list notation from string notation
    *
    * @param s string notation of the tree
    * @param t binary code of the actual node 
    * @param l accumulator for the list notation
    * @return list notation of the tree
    */
  @tailrec
  def parseTree(s:String, t:String, l:Tree): Tree = {
    if (s==null || s.isEmpty) l.reverse
    else {
      val ch = s.charAt(0)
      val tail = s.drop(1)
      ch match {
        case '[' => parseTree(tail, '0'+t, l)
        case ',' => parseTree(tail, '1'+t.drop(1), l)
        case ']' => parseTree(tail, t.drop(1), l)
        case _ => if ('0'<=s.charAt(1) && s.charAt(1)<= '9') {
          val v0 = s.charAt(0).toInt-48 
          val v1 = s.charAt(1).toInt-48
          parseTree(s.drop(2), t, (t.reverse, v0*10+v1) +: l) 
        } else {
          val v0 = s.charAt(0).toInt-48 
          parseTree(tail, t, (t.reverse, v0) +: l) 
        }
      }
    }
  }
  def buildTree(t:Tree): String = buildTree(t,"","")
  /**
    * construct the string notation of the tree from list notation - only for unit testing
    *
    * @param t list notation of the tree
    * @param s code of the actual node
    * @param r accumulator for the string notation
    * @return string notation of the tree
    */
  //@tailrec
  private def buildTree(t:Tree, s:String, r:String): String = {
    if (t.isEmpty && s.isEmpty) r.reverse else{
      val h = if (t.nonEmpty) t.head._1 else ""
      val n = if (t.nonEmpty)t.head._2 else 0
      val z = s.size-1
      if (h.size>s.size && h.take(z+1)==s) buildTree(t,s+'0','['+r)
      else if (h.size<s.size || h.take(z)!=s.take(z)) buildTree(t,s.take(z),']'+r)
      else if (h == s) buildTree(t.tail, s, n.toString.reverse+r)
      else if (h.take(z) == s.take(z)) buildTree(t, s.take(z)+'1', ','+r)
      else ""
    }
  }
  /**
    * The first two elements of the list are pair? 
    *
    * @param t tree in list notation
    * @return they are pair?
    */
  private def isPairAtTop(t:Tree): Boolean = {
    if (t.isEmpty || t.tail.isEmpty) false
    else {
      val c1 = t.head._1
      val c2 = t.tail.head._1
      if (c1.size!=c2.size) false
      else (c1.size>4) && (c1.dropRight(1)==c2.dropRight(1))
    }
  }
  def explodeTree(t:Tree): Tuple2[Tree, Boolean] = explodeTree(t, List[(String,Int)]())
  /**
   * Execute the explosion of the number
   * @param t input tree
   * @param acc accumulator tree
   * @return resulting tree - explode the first
   */
  //@tailrec
  def explodeTree(t: Tree, acc: Tree): Tuple2[Tree,Boolean] = {
    if (t.isEmpty) (acc.reverse, false)
    else {
      if (isPairAtTop(t)) {
        val lv = t.head._2 // left value
        val rv = t.tail.head._2 // right value
        val c = t.head._1.dropRight(1) // common prefix
        val newAcc = if (acc.nonEmpty) {
          val ahc = acc.head._1
          val ahn = acc.head._2
          (ahc, ahn + lv) +: acc.tail
        } else List[(String,Int)]()
        val newT = if (t.tail.tail.nonEmpty) {
          val tc = t.tail.tail.head._1
          val tv = t.tail.tail.head._2
          (c, 0) +: ((tc, tv + rv) +: t.tail.tail.tail)
        } else List[(String,Int)]((c, 0))
        val r1 = newAcc.reverse ++ newT
        (r1, true)
      } else explodeTree(t.tail, t.head +: acc)
    }
  }
  def splitTree(t:Tree): Tuple2[Tree, Boolean] = splitTree(t, List[(String,Int)]())
  /**
   * Execute the split of the number
   * @param t input tree
   * @param acc accumulator tree
   * @return resulting tree - split the first big number
   */
  //@tailrec
  private def splitTree(t: Tree, acc: Tree): Tuple2[Tree,Boolean] = {
    if (t.isEmpty) (acc.reverse, false)
    else {
      if (t.head._2>9) {
        val lv = t.head._2/2 // left value
        val rv = t.head._2-lv // right value
        val c = t.head._1 // common prefix
        val newT = (c+'0', lv) +: ((c+'1', rv) +: t.tail)
        val r1 = acc.reverse ++ newT
        (r1, true)
      } else splitTree(t.tail, t.head +: acc)
    }
  }
  /**
    * execute the explode and split steps of the addition
    *
    * @param t tree-number
    * @return tree-number in reduced form
    */
  def processTree(t:Tree): Tree = {
    val r = explodeTree(t)
    if (r._2) {
      processTree(r._1)
    }else {
      val r2 = splitTree(r._1)
      if (r2._2) { 
        processTree(r2._1)
      }
      else r2._1
    }
  }
  /**
    * Add two tree-numbers
    *
    * @param t1 first tree-number
    * @param t2 second tree-number
    * @return sum (as a tree-number)
    */
  def addTree(t1: Tree, t2: Tree): Tree = {
    val ht1 = for (p <- t1) yield ('0'+p._1, p._2)
    val ht2 = for (p <- t2) yield ('1'+p._1, p._2)
    val ht = ht1 ++ ht2 // create a pair from them
    processTree(ht)
  }
  /**
    * Add two tree-numbers in string form.
    *
    * @param s1 string notation of the first number
    * @param s2 string notation of the second number
    * @return sum (as a tree number)
    */
  def addPair(s1: String, s2: String) = {
    val t1 = parseTree(s1)
    val t2 = parseTree(s2)
    addTree(t1, t2)
  }
  /**
    * Summarize a list of trees
    *
    * @param l list of trees
    * @return sum (as a tree-number)
    */
  def addTrees(l: List[String]): Tree = {
    if (l.isEmpty) t
    else if (l.tail.isEmpty) parseTree(l.head)
    else {
      val t = parseTree(l.head)
      addTrees(t,l.tail)
    }
  }
  /**
    * Add to a tree a list of trees
    *
    * @param t tree
    * @param l list of trees
    * @return sum (as a tree-number)
    */
  @tailrec
  def addTrees(t: Tree, l: List[String]): Tree = {
    if (l.isEmpty) t
    else {
      val tn = parseTree(l.head)
      val ts = addTree(t, tn)
      addTrees(ts,l.tail)
    }
  }
  //@tailrec
  def magnitude(t:Tree):Int = magnitude(t, 0)
  /**
    * Calculate of the value of one digit
    *
    * @param s code of the node
    * @param v value of the digit
    * @return value of the digit
    */
  @tailrec
  def leaveValue(s:String, v:Int):Int = {
    if (s.isEmpty) v
    else if (s.charAt(0)=='1') leaveValue(s.drop(1), v*2) 
    else leaveValue(s.drop(1), v*3)
  }
  /**
    * Calculate the magnitude of a tree-number
    *
    * @param t tree
    * @param acc - accumulator for the sum of values of the digits
    * @return magnitude of the number
    */
  @tailrec
  private def magnitude(t:Tree, acc:Int):Int = {
    if (t.isEmpty) acc
    else {
      val v = leaveValue(t.head._1, t.head._2)
      magnitude(t.tail, acc+v)
    }
  }
  val items = fromResource("input18.txt").getLines().toList
  val t = addTrees(items)
  val m = magnitude(t)
  println(s"Part 1: $m")
  //------------------------------------------------------------- Part 2
  val ms = for(x <- items; y <- items if (x!=y)) yield(magnitude(addPair(x,y)))
  println(s"Part 2: ${ms.max}")
  
  
}