
package Dijkstra

import scala.annotation.tailrec
import scala.util.control.Breaks._

object Dijkstra extends App {

  type Graph = Map[Int,List[Edge]]
  type Distance = Array[Int]

  def shortestPath(graph:Array[Edge],start:Int,end:Int):Int = {

    val verbose:Boolean = true

    // Initial work
    val graphMap = createGraph(graph)
    val dist = Array.fill(graphMap.size)(Int.MaxValue)
    dist(start) = 0 // No distance to get to start
    val unvisited = (0 until graphMap.size).toSet

    if(verbose) println("starting work ... ")

    // Methods
   @tailrec def iterateHelper(unvisited:Set[Int],acc:Int=0) : Distance = {

      if(unvisited isEmpty) return dist

      if(acc == 6 ) break


      // Start with non-visited point with lowest distance
      val from = helper.whichMin(dist, i => unvisited.contains(i)) // .zipWithIndex.map( (x,i) => unvisited.contains(x) ) )

      from match {
        case None => throw new Exception(f"${dist.mkString(" ")} is empty")
        case Some(_) => if(verbose) println(f"selecting $from for this iteration")
      }

      val neighbors = graphMap(from.get)
      val currentDistance = dist(from.get)

      // update distance matrix
      neighbors.foreach( n => dist(n.to)  =  math.min(
                          dist(n.to), currentDistance + n.weight
                          )
                        )

      if(verbose){
        println(f"graphMap: $graphMap")
        println(f"unvisited $unvisited")
        println(f"neighbors are: $neighbors")
        println(f"updated distance: ${dist.mkString(" ")}")
        }

      iterateHelper(unvisited - from.get,acc+1)
      }

    val finalDistance = iterateHelper(unvisited)
    println(f"final distance is ${finalDistance.mkString("_")}")

    finalDistance(end)
  }


  def createGraph(graph:Array[Edge]):Graph = (graph ++ graph.map(_.reverse))
    .groupBy(e => e.from) // group by starting node
    .map( a => (a._1,a._2.toList) ) // to List

}