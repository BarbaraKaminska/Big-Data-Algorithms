import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.language.postfixOps


def mapper (e : List[(Int, List[Int])]) : ListBuffer[(Int, Int)] = 
    var res = ListBuffer[(Int, Int)]()
    var m = e.toIndexedSeq
    for x <- m do
        for v <- x(1) do
            res.addOne(v, x(0))
    res
        
def reducer (m : ListBuffer[(Int, Int)]) : ListBuffer[(Int, ListBuffer[Int])]=
    var res = ListBuffer[(Int, ListBuffer[Int])]()
    var g = m.groupBy(_(0))
    g.foreach{case(k,v) => res.addOne(k, v.map(x =>x(1)))}
    res

@main def graphInv = {
    val e = List(
        (1, List(2, 3, 4)),
        (3, List(1, 5, 4)),
        (2, List(5)),
        (5, List())
        )
    var m = mapper(e)
    println("Map: ")
    m.foreach(println)
    println("Reduce: ")
    reducer(m).foreach(println)
    //reducer(m)
    
}