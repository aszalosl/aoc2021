package aoc2021

class Day23Test4 extends org.scalatest.funsuite.AnyFunSuite {
  ignore("build0"){
    val s0 = Day23b(".......,CDDC,BCBD,ABAA,DACB",0)
    assert(s0.distance(1,1)==2)
    assert(s0.distance(0,1)==3)
    assert(s0.distance(6,4)==3)
    assert(s0.distance(0,4)==9)
    assert(!s0.freeRoom(1))
    assert(!s0.freeRoom(2))
    assert(!s0.freeRoom(3))
    assert(!s0.freeRoom(4))
    assert(s0.canGoToHallway(1,0))
    assert(s0.goToHallway(1,0)=="C......,DDC,BCBD,ABAA,DACB")
    assert(s0.canGoToHallway(1,3))
    assert(s0.goToHallway(1,3)=="...C...,DDC,BCBD,ABAA,DACB")
    assert(s0.canGoToHallway(1,5))
    assert(s0.goToHallway(1,5)==".....C.,DDC,BCBD,ABAA,DACB")
    assert(s0.canGoToHallway(4,0))
    assert(s0.goToHallway(4,0)=="D......,CDDC,BCBD,ABAA,ACB")
    assert(s0.canGoToHallway(4,5))
    assert(s0.goToHallway(4,5)==".....D.,CDDC,BCBD,ABAA,ACB")
    assert(s0.canGoToHallway(4,6))
    assert(s0.goToHallway(4,6)=="......D,CDDC,BCBD,ABAA,ACB")
    assert(s0.replace(0,'X') == "X......")
    assert(s0.replace(1,'X') == ".X.....")
    assert(s0.replace(3,'X') == "...X...")
    assert(s0.replace(6,'X') == "......X")
    assert(s0.successors.length==28)
  }
  ignore("build1"){
    val s1 = Day23b("...A...,CDDC,BCBD,BAA,DACB",5)
    assert(!s1.freeRoom(3) )
    assert(!s1.canGoToRoom(0))
    assert(!s1.canGoToRoom(3))
    assert(s1.canGoToHallway(1, 0))
    assert(!s1.canGoToHallway(1, 3))
    assert(!s1.canGoToHallway(1, 5))
    assert(s1.canGoToHallway(4,4))
    assert(!s1.canGoToHallway(4, 0))
    assert(s1.canGoToHallway(4, 6))
    assert(s1.successors.length==12)
  }
  ignore("build2"){
    val s2 = Day23b("AA..BBD,BDDA,BD,CCC,CA",0)
    assert(s2.freeRoom(3) )
    assert(!s2.freeRoom(2) )
    assert(!s2.canGoToRoom(0))
    assert(!s2.canGoToRoom(4))
    assert(s2.canGoToHallway(1, 3))
    assert(s2.canGoToHallway(1, 2))
    assert(!s2.canGoToHallway(1, 1))
    assert(!s2.canGoToHallway(4,4))
    assert(s2.canGoToHallway(2, 3))
    assert(s2.canGoToHallway(2, 2))
    assert(s2.distance(1,2)==5)
    assert(s2.distance(1,3)==6)
    assert(s2.distance(5,4)==3)
    assert(s2.distance(4,4)==3)
    assert(s2.distance(4,3)==2)
    assert(s2.distance(4,2)==5)
    assert(s2.successors.toSet == Set(Day23b("AAB.BBD,DDA,BD,CCC,CA",20),
        Day23b("AAB.BBD,BDDA,D,CCC,CA",40),
        Day23b("AA.BBBD,DDA,BD,CCC,CA",40),
        Day23b("AA.BBBD,BDDA,D,CCC,CA",40)))
  }
  ignore("build3"){
    val s3 = Day23b("AAD.BBD,BDDA,B,CCC,CA",45635)
    assert(!s3.freeRoom(4) )
    assert(s3.canGoToRoom(4))
    assert(s3.goToRoom(4)=="AAD..BD,BDDA,BB,CCC,CA")
    assert(!s3.canGoToRoom(5))
    assert(!s3.canGoToHallway(1, 1))
    assert(!s3.canGoToHallway(2, 1))
    assert(!s3.canGoToHallway(2, 3))
    assert(!s3.canGoToHallway(3,4))
    assert(!s3.canGoToHallway(3, 3))
    assert(!s3.canGoToHallway(4, 3))
  }
  ignore("build3a"){
    val s3a = Day23b("AAB.DBD,BDDA,B,CCC,CA",45635)
    assert(!s3a.freeRoom(4) )
    assert(s3a.canGoToRoom(2))
    assert(s3a.goToRoom(2)=="AA..DBD,BDDA,BB,CCC,CA")
    assert(!s3a.canGoToRoom(4))
    assert(!s3a.canGoToHallway(1, 1))
    assert(!s3a.canGoToHallway(2, 1))
    assert(!s3a.canGoToHallway(2, 2))
    assert(!s3a.canGoToHallway(3,4))
    assert(!s3a.canGoToHallway(3, 3))
    assert(!s3a.canGoToHallway(4, 3))
  }
  ignore("build4"){
    val s4 = Day23b("AA...AD,DDA,BBBB,CCCC,D",0)
    assert(!s4.freeRoom(1) )
    assert(s4.freeRoom(4) )
    assert(!s4.canGoToRoom(1))
    assert(!s4.canGoToRoom(5))
    assert(s4.canGoToHallway(1, 2))
    assert(s4.canGoToHallway(1, 3))
    assert(!s4.canGoToHallway(4, 3))
    assert(!s4.canGoToHallway(4,4))
    assert(!s4.canGoToHallway(2, 2))
    assert(!s4.canGoToHallway(3, 3))
    assert(s4.distance(1,1)==2)
    assert(s4.distance(1,4)==10)
    assert(s4.distance(5,4)==4)
    assert(s4.distance(5,1)==8)
    assert(s4.successors.toSet == Set(Day23b("AAD..AD,DA,BBBB,CCCC,D",3000),
      Day23b("AA.D.AD,DA,BBBB,CCCC,D",5000),
      Day23b("AA..DAD,DA,BBBB,CCCC,D",7000)))
  }
}
class Day23Test2 extends org.scalatest.funsuite.AnyFunSuite {
  test("build5") {
    val s5 = Day23b("CCBDA.D,,,A,B", 6622)
    assert(s5.freeRoom(1))
    assert(s5.freeRoom(2))
    assert(!s5.freeRoom(3))
    assert(!s5.freeRoom(4))
    assert(!s5.canGoToRoom(0))
    assert(!s5.canGoToRoom(1))
    assert(s5.canGoToRoom(2))
    assert(!s5.canGoToRoom(3))
    assert(!s5.canGoToRoom(4))
    assert(!s5.canGoToRoom(5))
    assert(!s5.canGoToRoom(6))
    assert(!s5.canGoToHallway(1, 2))
    assert(!s5.canGoToHallway(1, 3))
    assert(s5.canGoToHallway(4, 5))
    assert(!s5.canGoToHallway(4, 4))
    assert(s5.successors.toSet == Set(Day23b("CC.DA.D,,B,A,B", 6652),
      Day23b("CCBDABD,,,A,", 6652)))
  }
  test("build6") {
    val s = Day23b("CCBDABD,,,A,", 6652)
    assert(s.freeRoom(1))
    assert(s.freeRoom(2))
    assert(!s.freeRoom(3))
    assert(s.freeRoom(4))
    assert(s.canGoToRoom(2))
    assert(!s.canGoToRoom(3))
    assert(!s.canGoToRoom(0))
    assert(!s.canGoToRoom(1))
    assert(!s.canGoToRoom(4))
    assert(!s.canGoToRoom(5))
    assert(!s.canGoToRoom(6))
    assert(!s.canGoToHallway(1, 2))
    assert(!s.canGoToHallway(1, 3))
    assert(!s.canGoToHallway(4, 5))
    assert(!s.canGoToHallway(4, 4))
    assert(s.successors.toSet == Set(Day23b("CC.DABD,,B,A,", 6682)))
  }
}