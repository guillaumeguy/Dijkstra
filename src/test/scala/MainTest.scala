/**
  * Created by Guy Guillaume on 2/27/2018
  * Copyright Guillaume Guy
  */


import Dijkstra._
import org.scalatest._

class MainTest extends FlatSpec with Matchers {


  "whichMin" should "return the index of the min value" in {

    helper.whichMin(Array(10,1,2,3), ind => ind == 3  ) should be (Some(3))

    helper.whichMin(Array(1,3,1,3), ind => ind < 2  ) should be (Some(0))

  }

  "Problem 1: Shortest path" should "equals 20" in {

   val inputs = Array(
                   Edge(1,2,7)
                  ,Edge(1,3,9)
                  ,Edge(1,6,14)
                  ,Edge(2,3,10)
                  ,Edge(2,4,15)
                  ,Edge(3,4,11)
                  ,Edge(3,6,2)
                  ,Edge(4,5,6)
                  ,Edge(5,6,9)
    ).map( x => new Edge(x.from -1 , x.to - 1, x.weight))

    Dijkstra.shortestPath(graph=inputs,start=0,end=5-1) should be (20)


  }

}