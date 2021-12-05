package aoc2021
import scala.io.Source.fromResource

object Day04 extends App {
    type Row = List[Int]
    type Matrix = List[Row]
    val bingoSize = 5
    /** Takes the lines and split them into list of numbers
     * 
     * @param ls list of strings containing numbers
     * @param nss accumulator of matrix of numbers
     * @return a square form matrix of numbers
     */ 
    def splitBingo(ls:List[String], nss: Matrix):Matrix = {
        if (ls.isEmpty) nss
        else {
            val ns = " +".r.split(ls.head.trim).map(_.toInt).toList
            splitBingo(ls.tail, ns :: nss)
        }
    }
    /** Process the rest of the file, chops into matrices and collects the rows and columns.
     * 
     * @param ls list of strings containing numbers
     * @param rs accumulator for the rows
     * @param cs accumulator for the columns
     * @return the pair of the list of rows and columns
     */
    def rowsColums(ls:List[String], rs:Matrix, cs: Matrix): Tuple2[Matrix,Matrix] = {
        if (ls.isEmpty) (rs,cs)
        else {
            val top = ls.tail.take(bingoSize)
            val matrix = splitBingo(top, List())
            rowsColums(ls.drop(bingoSize+1), matrix++rs, matrix.transpose++cs)
        }
    }
    val items = fromResource("input04.txt").getLines().toList
    val numbers = items.head.split(',').map(_.toInt).toList
    val (rs,cs) = rowsColums(items.tail, List(), List())
    //print(rs, cs)
    def findFirst(i:Int): Int = {
        val nums = numbers.take(i).toSet
        if (rs.exists(_.toSet subsetOf nums)) {
            val line = (rs.map(_.toSet subsetOf nums)).indexOf(true)
            val firstLine = line - (line%bingoSize)
            val table = (rs(firstLine) ++ rs(firstLine+1)++rs(firstLine+2)++rs(firstLine+3)++rs(firstLine+4)).toSet
            val freeSum = (table -- nums).toList.sum
            numbers(i-1)*freeSum
        }
        else if (rs.exists(_.toSet subsetOf nums)) {
            val line = (cs.map(_.toSet subsetOf nums)).indexOf(true)
            val firstLine = line - (line%bingoSize)
            val table = (cs(firstLine) ++ cs(firstLine+1)++cs(firstLine+2)++cs(firstLine+3)++cs(firstLine+4)).toSet
            val freeSum = (table -- nums).toList.sum
            numbers(i-1)*freeSum
        }
        else findFirst(i+1)
    }
    def findLast(i:Int): Int = {
        val nums = numbers.take(i).toSet
        val fullRows = rs.map(_.toSet subsetOf nums).zipWithIndex.filter(p => p._1).map(_._2)
        val fullCols = cs.map(_.toSet subsetOf nums).zipWithIndex.filter(p => p._1).map(_._2)
        val tables = fullRows.map(_/bingoSize).toSet ++ fullCols.map(_/bingoSize).toSet
        if (tables.size < rs.size/bingoSize) {
            val allTables = (0 until i).toSet
            val finalTable = allTables.diff(tables).head
            val finalNums = nums + numbers(i)
            val finalRow = finalTable*bingoSize
            val freeNums = (rs(finalRow) ++ rs(finalRow+1)++rs(finalRow+2)++rs(finalRow+3)++rs(finalRow+4)).toSet.diff(finalNums)
            numbers(i)*freeNums.toList.sum 
        }
        else findLast(i-1) 
        
    }

    println("First part: " + findFirst(5).toString())
    println("Second part: " + findLast(numbers.size-1).toString())
}
