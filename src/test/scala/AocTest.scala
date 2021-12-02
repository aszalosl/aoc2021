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

}
