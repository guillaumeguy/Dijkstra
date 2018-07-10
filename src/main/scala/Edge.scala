/**
  * Created by Guy Guillaume on 2/27/2018
  * Copyright Guillaume Guy
  */
// case class Book(isbn: String)

package Dijkstra

case class Edge (from:Int  ,to:Int  ,weight:Int){

  def reverse:Edge = Edge(from=this.to,to=this.from,weight=weight)

}