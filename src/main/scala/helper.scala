package Dijkstra
import Dijkstra.Distance
/**
  * Created by Guy Guillaume on 2/27/2018
  * Copyright Guillaume Guy
  */

object helper {

// Select the index of the lower element whose index meets condition f
  def whichMin(x:Distance, f: Int => Boolean):Option[Int] = {

    val verbose = false

    val result = x.zipWithIndex.filter(  a => f(a._2) )

    result match {
      case arr if arr.isEmpty => None
      case arr => {
        val finalMin = arr.foldLeft( (arr(0)._1,arr(0)._1) )(
          (localmin,potential) =>
            // When to update
            if( potential._1 <= localmin._1) potential  else  localmin
        )

       if(verbose) println(f"${x.mkString(" ")} min is at index ${finalMin._2}")
        Some(finalMin._2)
      }
    }

  }

}