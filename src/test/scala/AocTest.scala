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

}
