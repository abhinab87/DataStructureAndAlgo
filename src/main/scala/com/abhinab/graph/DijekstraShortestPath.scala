package com.abhinab.graph

object DijekstraShortestPath {
  val v = 9
  def minDistance(dist: Array[Int],sptSet: Array[Boolean]): Int={
    var min = Integer.MAX_VALUE
    var min_index = -1
    for (i <- 0 until v) {
      if (sptSet(i) == false && dist(i) <= min) {
        min = dist(i);
        min_index = i;
      }
    }
    min_index
  }

  def printSolution(dist:Array[Int], n: Int): Unit={
    System.out.println("Vertex   Distance from Source");
    for (i <- 0 until v)
    System.out.println(i+" tt "+dist(i));
  }

  def dijkstra(graph: Array[Array[Int]], src:Int): Unit ={
    var dist = Array[Int]()
    var sptSet = Array[Boolean]()

    for (i <- 0 until v){
      dist = dist :+ Integer.MAX_VALUE;
      sptSet = sptSet :+ false;
    }
    dist(src) = 0
    for (count <- 0 until v-1) {
      // Pick the minimum distance vertex from the set of vertices
      // not yet processed. u is always equal to src in first
      // iteration.
      val u = minDistance(dist, sptSet);

      // Mark the picked vertex as processed
      sptSet(u) = true;

      // Update dist value of the adjacent vertices of the
      // picked vertex.
      for (i <- 0 until v) {
        println("!! "+dist(u))
        // Update dist[v] only if is not in sptSet, there is an
        // edge from u to v, and total weight of path from src to
        // v through u is smaller than current value of dist[v]
        if (!sptSet(i) && graph(u)(i) != 0 && dist(u) != Integer.MAX_VALUE && dist(u) + graph(u)(i) < dist(i))
          dist(i) = dist(u) + graph(u)(i)
      }
    }
    printSolution(dist, v)
  }

  def main(args: Array[String]): Unit = {
    var graph = Array[Array[Int]]()
    graph = Array(Array(0, 4, 0, 0, 0, 0, 0, 8, 0),
      Array(4, 0, 8, 0, 0, 0, 0, 11, 0),
      Array(0, 8, 0, 7, 0, 4, 0, 0, 2),
      Array(0, 0, 7, 0, 9, 14, 0, 0, 0),
      Array(0, 0, 0, 9, 0, 10, 0, 0, 0),
      Array(0, 0, 4, 14, 10, 0, 2, 0, 0),
      Array(0, 0, 0, 0, 0, 2, 0, 1, 6),
      Array(8, 11, 0, 0, 0, 0, 1, 0, 7),
      Array(0, 0, 2, 0, 0, 0, 6, 7, 0))

    dijkstra(graph,0)
  }
}
