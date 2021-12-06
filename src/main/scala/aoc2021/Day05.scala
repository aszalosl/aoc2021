package aoc2021
import scala.io.Source.fromResource

object Day05 extends App {
    type Pair = Tuple2[Int, Int]
    val items = fromResource("input05.txt").getLines().toList
    val pathPattern = """(\d+),(\d+) -> (\d+),(\d+)""".r
    val paths = items.map {
            case pathPattern(xf,yf,xt,yt) => (xf.toInt,yf.toInt,xt.toInt,yt.toInt)
        }
    val horizontal = paths.filter(z => z._2 == z._4)
    .map(_ match {
        case w if w._1 > w._3 => (w._3,w._2,w._1,w._2)
        case w => w
     })
    val vertical = paths.filter(z => z._1 == z._3)
    .map(_ match {
        case w if w._2 > w._4 => (w._1,w._4,w._1,w._2)
        case w => w
     })
    val vllt = vertical.map(t => (t._2 to t._4).map(y => (t._1,y))).flatten  //XYXZ
    val hllt = horizontal.map(t => (t._1 to t._3).map(x => (x,t._2))).flatten  //XYZY

    def myorder(x:Pair,y:Pair) =
        (x._1<y._1) || (x._1==y._1) && (x._2<y._2)
    val vh = (vllt ++ hllt).sortWith{myorder}
    def counter(x:Pair, y:Pair, zs:List[Pair], c:Int):Int = {
        if (zs.isEmpty) c
        else if (x!=y && y==zs.head) counter(y,zs.head,zs.tail,c+1)
        else counter(y,zs.head,zs.tail,c)
    }
    val vh0 = vh(0)
    val vh1 = vh(1)
    val solution1 = if (vh0==vh1) counter(vh0, vh1, vh.tail.tail,1)
    else counter(vh0, vh1, vh.tail.tail,0)
    println("Part 1: " + solution1.toString)
    // Part 2
    val diagonal = paths.filter(z => (z._1-z._3).abs == (z._2-z._4).abs)
    .map(_ match {
        case w if w._1 > w._3 => (w._3,w._4,w._1,w._2)
        case w => w
    })
    val dllt = diagonal.map(t => (t._1 to t._3).map(x => (x,t._2+(t._4-t._2).signum*(x-t._1)))).flatten  //XYXZ
    val vhd = (vllt++hllt++dllt).sortWith(myorder)
    val vhd0 = vhd(0)
    val vhd1 = vhd(1)
    val solution2 = if (vhd0==vhd1) counter(vhd0, vhd1, vhd.tail.tail,1)
    else counter(vhd0, vhd1, vhd.tail.tail,0)
    println("Part 1: " + solution2.toString)
}
