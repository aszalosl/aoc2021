import org.scalatest.Ignore
import aoc2021.Day18

class Day18Test extends org.scalatest.funsuite.AnyFunSuite {
  test("build0"){
    val t0 = List(("00000",9), ("00001",8), ("0001",1), ("001",2), ("01",3), ("1",4))
    assert(aoc2021.Day18.buildTree(t0) == "[[[[[9,8],1],2],3],4]")

  }
  test("build"){
    val t0 = aoc2021.Day18.parseTree("[[[[[9,8],1],2],3],4]")
    assert(aoc2021.Day18.buildTree(t0) == "[[[[[9,8],1],2],3],4]")
    
    val t1 = aoc2021.Day18.parseTree("[7,[6,[5,[7,0]]]]")
    assert(aoc2021.Day18.buildTree(t1) == "[7,[6,[5,[7,0]]]]")

    val t2 = aoc2021.Day18.parseTree("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    assert(aoc2021.Day18.buildTree(t2) == "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
  
    val t3 = aoc2021.Day18.parseTree("[[[[0,7],4],[15,[0,13]]],[1,1]]")
    assert(aoc2021.Day18.buildTree(t3) == "[[[[0,7],4],[15,[0,13]]],[1,1]]")

    val t4 = aoc2021.Day18.parseTree("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
    assert(aoc2021.Day18.buildTree(t4) == "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
  }
  test("explode"){
    val t0 = aoc2021.Day18.parseTree("[[[[[9,8],1],2],3],4]")
    val e0 = aoc2021.Day18.explodeTree(t0)
    assert(aoc2021.Day18.buildTree(e0._1) == "[[[[0,9],2],3],4]")

    val t1 = aoc2021.Day18.parseTree("[7,[6,[5,[4,[3,2]]]]]")
    val e1 = aoc2021.Day18.explodeTree(t1)
    assert(aoc2021.Day18.buildTree(e1._1) == "[7,[6,[5,[7,0]]]]")

    val t2 = aoc2021.Day18.parseTree("[[6,[5,[4,[3,2]]]],1]")
    val e2 = aoc2021.Day18.explodeTree(t2)
    assert(aoc2021.Day18.buildTree(e2._1) == "[[6,[5,[7,0]]],3]")
    
    val t3 = aoc2021.Day18.parseTree("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
    val e3 = aoc2021.Day18.explodeTree(t3)
    assert(aoc2021.Day18.buildTree(e3._1) == "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    
    val t4 = aoc2021.Day18.parseTree("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    val e4 = aoc2021.Day18.explodeTree(t4)
    assert(aoc2021.Day18.buildTree(e4._1) == "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  }
  test("split"){
    val t0 = aoc2021.Day18.parseTree("[[[[0,7],4],[15,[0,13]]],[1,1]]")
    val e0 = aoc2021.Day18.splitTree(t0)
    assert(aoc2021.Day18.buildTree(e0._1) == "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")

    val t1 = aoc2021.Day18.parseTree("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
    val e1 = aoc2021.Day18.splitTree(t1)
    assert(aoc2021.Day18.buildTree(e1._1) == "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
  }
  test("process"){
    val t0 = aoc2021.Day18.parseTree("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
    val e0 = aoc2021.Day18.processTree(t0)
    assert(aoc2021.Day18.buildTree(e0) == "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  }
  test("add"){
    val l0 = List("[1,1]","[2,2]","[3,3]","[4,4]")
    val e0 = aoc2021.Day18.addTrees(l0)
    assert(aoc2021.Day18.buildTree(e0) == "[[[[1,1],[2,2]],[3,3]],[4,4]]")
    
    val l1 = List("[1,1]","[2,2]","[3,3]","[4,4]","[5,5]")
    val e1 = aoc2021.Day18.addTrees(l1)
    assert(aoc2021.Day18.buildTree(e1) == "[[[[3,0],[5,3]],[4,4]],[5,5]]")
    
  
    val l2 = List("[1,1]","[2,2]","[3,3]","[4,4]","[5,5]","[6,6]")
    val e2 = aoc2021.Day18.addTrees(l2)
    assert(aoc2021.Day18.buildTree(e2) == "[[[[5,0],[7,4]],[5,5]],[6,6]]")
  }
  test("magnitude"){
    val t0 = aoc2021.Day18.parseTree("[[1,2],[[3,4],5]]")
    assert(aoc2021.Day18.magnitude(t0)== 143)
    val t1 = aoc2021.Day18.parseTree("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    assert(aoc2021.Day18.magnitude(t1)== 1384)
    val t2 = aoc2021.Day18.parseTree("[[[[1,1],[2,2]],[3,3]],[4,4]]")
    assert(aoc2021.Day18.magnitude(t2)== 445)
    val t3 = aoc2021.Day18.parseTree("[[[[3,0],[5,3]],[4,4]],[5,5]]")
    assert(aoc2021.Day18.magnitude(t3)== 791)
    val t4 = aoc2021.Day18.parseTree("[[[[5,0],[7,4]],[5,5]],[6,6]]")
    assert(aoc2021.Day18.magnitude(t4)== 1137)
    val t5 = aoc2021.Day18.parseTree("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
    assert(aoc2021.Day18.magnitude(t5)== 3488)
  }
 
}