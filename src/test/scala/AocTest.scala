import org.scalatest.Ignore

@Ignore
class AocTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Day01.increments") {
    assert(aoc2021.Day01.increments(List(1,2,3)) === 2)
    assert(aoc2021.Day01.increments(List(3,2,1)) === 0)
    assert(aoc2021.Day01.increments(List(1,3,2)) === 1)
    assert(aoc2021.Day01.increments(List(2,1,3)) === 1)
  }
  test("Day02.fold") {
    val ps = List((5,0),(0,5),(8,0),(0,-3),(0,8),(2,0))
    val (a,b) = aoc2021.Day02.doubleSum(ps)
    assert(a === 15)
    assert(b === 10)
    val (c,d,e) = aoc2021.Day02.secondMethod(ps)
    assert(c === 15)
    assert(d === 60)
  }
  test("Day03.summit"){
    val bits = List("00100", "11110", "10110", "10111","10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
    assert(aoc2021.Day03.gammaEpsilon(bits)==198)
    assert(aoc2021.Day03.generatorRating(0,bits,aoc2021.Day03.oneStepO2)=="10111")
    assert(aoc2021.Day03.generatorRating(0,bits,aoc2021.Day03.oneStepCO2)=="01010")
  }
  test("Day04.bingo"){
    val top = List("83 40 67 98  4","50 74 31 30  3","75 64 79 61  5","12 59 26 25 72","36 33 18 54 10")
    val matrix = (aoc2021.Day04.splitBingo(top,List()))
    assert(matrix == List(List(36, 33, 18, 54, 10), List(12, 59, 26, 25, 72), List(75, 64, 79, 61, 5), List(50, 74, 31, 30, 3), List(83, 40, 67, 98, 4)))
  }
  test("Day06"){
    assert(aoc2021.Day06.nextFish(0,1,1,2,1,0,0,0,0,18)== 26)
    assert(aoc2021.Day06.nextFish(0,1,1,2,1,0,0,0,0,80)== 5934)
    assert(aoc2021.Day06.nextFish(0,1,1,2,1,0,0,0,0,256)== BigInt(26984457539L)) 
  }
test("Day07"){
    assert(aoc2021.Day07.sumNum(11)== 66)
    assert(aoc2021.Day07.sumNum(3)== 6)
    assert(aoc2021.Day07.sumNum(2)== 3)
    assert(aoc2021.Day07.sumNum(9)== 45)
    assert(aoc2021.Day07.sumNum(10)== 55)
  }
test("Day08"){
    assert(aoc2021.Day08.process("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab", "cdfeb fcadb cdfeb cdbaf") == 5353)
  }
}
@Ignore
class Day11Test extends org.scalatest.funsuite.AnyFunSuite {
  val smallTable = aoc2021.Day11.makeTable(List("11111","19991","19191","19991","11111"),5,5)
  val sampleTable = aoc2021.Day11.makeTable(List("5483143223","2745854711","5264556173","6141336146","6357385478","4167524645","2176841721","6882881134","4846848554","5283751526"), 10, 10)
    
test("rewrite"){
    val ls = aoc2021.Day11.tableToString(smallTable,5,5)
    assert(ls == List("11111","19991","19191","19991","11111"),"rewriting")
  }
test("one step on the small table"){
    val s1 = aoc2021.Day11.oneStep(smallTable)
    assert(aoc2021.Day11.tableToString(s1._1,5,5) == List("34543", "40004", "50005", "40004", "34543"))
  }
test("one step on the small table - variant"){
    val s1 = aoc2021.Day11.manySteps(smallTable,1,0)
    assert(aoc2021.Day11.tableToString(s1._1,5,5) == List("34543", "40004", "50005", "40004", "34543"))
  }
ignore("Day11 small, two steps"){
    val s2 = aoc2021.Day11.manySteps(smallTable,2,0)
    assert(aoc2021.Day11.tableToString(s2._1,5,5) == List("45654", "51115", "61116", "51115", "45654"))
  }

  ignore("Day11-2"){

  }
}
class Day16Test extends org.scalatest.funsuite.AnyFunSuite {
  test("from binary to integer"){
    assert(0 == aoc2021.Day16.bin2int("0",0))
    assert(0 == aoc2021.Day16.bin2int("000000",0))
    assert(1 == aoc2021.Day16.bin2int("01",0))
    assert(5 == aoc2021.Day16.bin2int("101",0))
    assert(15 == aoc2021.Day16.bin2int("1111",0))
  }
  test("part 1"){
    assert((2021,21) == aoc2021.Day16.decodeLiteral("110100101111111000101000",6,0) )
    val t0 = aoc2021.Day16.string2bin("EE00D40C823060")
    assert((14,51) == aoc2021.Day16.parse1(t0,0,56) )
    val t1 = aoc2021.Day16.string2bin("8A004A801A8002F478")
    assert((16,69) == aoc2021.Day16.parse1(t1,0,72) )
    val t2 = aoc2021.Day16.string2bin("620080001611562C8802118E34")
    assert((12,102) == aoc2021.Day16.parse1(t2,0,104) )
    val t3 = aoc2021.Day16.string2bin("C0015000016115A2E0802F182340")
    assert((23,106) == aoc2021.Day16.parse1(t3,0,112) )
    val t4 = aoc2021.Day16.string2bin("A0016C880162017C3686B18A3D4780")
    assert((31,113) == aoc2021.Day16.parse1(t4,0,120) )
  }
  test("part 2"){
    val t0 = aoc2021.Day16.string2bin("C200B40A82")
    assert(3 == aoc2021.Day16.parse2(t0,0,t0.size*4)._1 )
    val t1 = aoc2021.Day16.string2bin("04005AC33890")
    assert(54 == aoc2021.Day16.parse2(t1,0,t1.size*4)._1 )
    val t2 = aoc2021.Day16.string2bin("880086C3E88112")
    assert(7 == aoc2021.Day16.parse2(t2,0,t2.size*4)._1 )
    val t3 = aoc2021.Day16.string2bin("CE00C43D881120")
    assert(9 == aoc2021.Day16.parse2(t3,0,t3.size*4)._1 )
    val t4 = aoc2021.Day16.string2bin("D8005AC2A8F0")
    assert(1 == aoc2021.Day16.parse2(t4,0,t4.size*4)._1 )
    val t5 = aoc2021.Day16.string2bin("F600BC2D8F")
    assert(0 == aoc2021.Day16.parse2(t5,0,t5.size*4)._1 )
    val t6 = aoc2021.Day16.string2bin("9C005AC2F8F0")
    assert(0 == aoc2021.Day16.parse2(t6,0,t6.size*4)._1 )
    val t7 = aoc2021.Day16.string2bin("9C0141080250320F1802104A08")
    assert(1 == aoc2021.Day16.parse2(t7,0,t7.size*4)._1 )
  }
}
