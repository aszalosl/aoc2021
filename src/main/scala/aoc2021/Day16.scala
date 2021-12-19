package aoc2021

import scala.annotation.tailrec
import scala.io.Source.fromResource
import scala.math.BigInt
object Day16 extends App {
  val input = fromResource("input16.txt").getLines().next
  /**
    * "binary" representation of the hexadecimal string
    *
    * @param s hexadecimal "numbers"
    * @return binary "numbers"
    */
  def string2bin(s:String) = {
    /*TODO: Question - can I move constant hex2bin outside to
            speed up the execution, while the unit test is working?
    */
    val hex2bin = Map('0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011", '4' -> "0100", '5' -> "0101", '6' -> "0110", '7' -> "0111",
                      '8' -> "1000", '9' -> "1001", 'A' -> "1010", 'B' -> "1011", 'C' -> "1100", 'D' -> "1101", 'E' -> "1110", 'F' -> "1111")
    s.map(hex2bin(_)).flatten.mkString
  }
  /**
    * Integer value of a binary number
    *
    * @param s binary number 
    * @param i accumulator variable (starts with 0)
    * @return integer value of its input
    */
  @tailrec
  def bin2int(s:String, i: Int):Int = 
    if (s.isEmpty()) i
    else {if (s(0)=='0') bin2int(s.substring(1), 2*i) else bin2int(s.substring(1), 2*i+1)}

  /**
    * Decode an encoded literal packet
    *
    * @param s string of every packets
    * @param b start position in s of remaining literal parts
    * @param i accumulator for decoded value
    * @return pair of the value and end position
    */
  @tailrec
  def decodeLiteral(s:String, b:Int, i:Int): Tuple2[Int,Int] = {
    val i0 = i*16 + bin2int(s.substring(b+1,b+5),0)
    if (s(b)=='0') {  (i0, b+5) } else { decodeLiteral(s, b+5, i0) }
  }
  /**
    * Decode subpackets within the b-e limits
    *
    * @param s the sequence of packets
    * @param b starting position of current packet
    * @param e theoretical end position of all packets at this level
    * @param ns collected values from the packets
    * @return the collected values and the real end position 
    */
  @tailrec
  def parseLength(s:String, b: Int, e:Int, ns:List[Int]): Tuple2[List[Int],Int] = {
    if (b+5<e) {
      val ne = parse1(s, b, e)
      parseLength(s, ne._2, e, ns :+ ne._1)
    } else (ns, b)
  }
  /**
    * Read n subpackets
    *
    * @param s the sequence of packets
    * @param b starting position of current packet
    * @param e theoretical end position of all packets at this level
    * @param n number of remaining subpackets
    * @param ns collected values from the packets
    * @return the collected values and the real end position
    */ 
  @tailrec
  def parseNum(s:String, b: Int, e:Int, n:Int, ns:List[Int]):Tuple2[List[Int],Int] = {
    if (n<=0) (ns, b)
    else {
      val ne = parse1(s, b, e)
      parseNum(s, ne._2, e, n-1, ns :+ ne._1)
    }
  }
  /**
    * Parsing of a string based on rules of part 1
    *
    * @param s the input string
    * @param b start position of the packet
    * @param e end position of the packet
    * @return the value of the message
    */
  def parse1(s:String, b:Int, e:Int): Tuple2[Int,Int] = {
      val pVersion = bin2int(s.substring(b,b+3),0)
      val pType = bin2int(s.substring(b+3,b+6),0)
      if (pType == 4) { // literal type
        val ve = decodeLiteral(s, b+6, 0) //unnecessary computing
        (pVersion,ve._2)
      } else {  //operator type
        if (s(b+6)=='0') { //
          val len = bin2int(s.substring(b+7,b+22),0)
          val vse = parseLength(s,b+22,b+22+len,List[Int]())
          (vse._1.sum+pVersion,vse._2) 
        } else {
          val num = bin2int(s.substring(b+7,b+18),0)
          val vse = parseNum(s,b+18,e,num,List[Int]())
          (vse._1.sum+pVersion,vse._2)
        }
      } 
  }
  //----------------------------------------------------------------- Part 2
  // we need very big numbers!
  
  @tailrec
  def decodeLiteral2(s:String, b:Int, i:BigInt): Tuple2[BigInt,Int] = {
    val i0 = i*BigInt(16) + bin2int(s.substring(b+1,b+5),0)
    if (s(b)=='0') {  (i0, b+5) } else { decodeLiteral2(s, b+5, i0) }
  }
  /** same as before, just we need a copy as it is a corutine. 
   * TODO: other kind of solution?
  */
  @tailrec
  def parseLength2(s:String, b: Int, e:Int, ns:List[BigInt]): Tuple2[List[BigInt],Int] = {
    if (b+5<e) {
      val ne = parse2(s, b, e)
      parseLength2(s, ne._2, e, ns :+ ne._1)
    } else (ns, b)
  }
  /** same as before, just we need a copy as it is a corutine. */
  @tailrec
  def parseNum2(s:String, b: Int, e:Int, n:Int, ns:List[BigInt]):Tuple2[List[BigInt],Int] = {
    if (n<=0) (ns, b)
    else {
      val ne = parse2(s, b, e)
      parseNum2(s, ne._2, e, n-1, ns :+ ne._1)
    }
  }
  def opcode(t:Int,lst:List[BigInt]):BigInt = {
    val r = t match {
      case 0 => lst.sum
      case 1 => lst.product
      case 2 => lst.min
      case 3 => lst.max
      case 5 => if (lst(0)>lst(1)) BigInt(1) else BigInt(0) //greater
      case 6 => if (lst(0)<lst(1)) BigInt(1) else BigInt(0) //less
      case 7 => if (lst(0)==lst(1)) BigInt(1) else BigInt(0) //equal
    }
    r
  }
  /**
    * Parsing of a string based on rules of part 1
    *
    * @param s the input string
    * @param b start position of the packet
    * @param e end position of the packet
    * @return the value of the message
    */
  def parse2(s:String, b:Int, e:Int): Tuple2[BigInt,Int] = {
      val pVersion = bin2int(s.substring(b,b+3),0)
      val pType = bin2int(s.substring(b+3,b+6),0)
      if (pType == 4) { // literal type
        val ve = decodeLiteral2(s, b+6, BigInt(0))
        (ve._1,ve._2)
      } else {  //operator type
        if (s(b+6)=='0') { //
          val len = bin2int(s.substring(b+7,b+22),0)
          val vse = parseLength2(s,b+22,b+22+len,List[BigInt]())
          val r = opcode(pType, vse._1)
          (r, vse._2) 
        } else {
          val num = bin2int(s.substring(b+7,b+18),0)
          val vse = parseNum2(s,b+18,e,num,List[BigInt]())
          val r = opcode(pType, vse._1)
          (r, vse._2) 
        }
      } 
  }

  val p = string2bin(input)
  val p1 = parse1(p, 0, p.size)
  println(s"Part 1: ${p1._1}")
  val p2 = parse2(p, 0, p.size)
  println(s"Part 2: ${p2._1}")
}