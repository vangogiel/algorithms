package io.vangogiel.algorithms.deepfirstsearch

import scala.collection.mutable

class DeepFirstSearchAlgorithm[S](edges: Seq[Edge[S]]) {
  private val adjacencyList: Map[S, Seq[Edge[S]]] = buildAdjacencyList(edges)

  private def buildAdjacencyList(edges: Seq[Edge[S]]): Map[S, Seq[Edge[S]]] = {
    val nodeEdges = edges.flatMap { edge =>
      Seq(
        edge.nodeA -> edge,
        edge.nodeB -> edge
      )
    }
    nodeEdges.groupBy { case (node, _) => node }.view.mapValues(_.map { case (_, edge) => edge }).toMap
  }

  def allPaths(start: Node[S], end: Node[S]): List[List[Edge[S]]] = {
    val visited: mutable.Set[S] = mutable.Set()
    val currentPath: mutable.ListBuffer[Edge[S]] = mutable.ListBuffer()
    val allPaths: mutable.ListBuffer[List[Edge[S]]] = mutable.ListBuffer()

    findAllPaths(start.id, end.id, visited, currentPath, allPaths)

    allPaths.toList
  }

  private def findAllPaths(
      current: S,
      end: S,
      visited: mutable.Set[S],
      currentPath: mutable.ListBuffer[Edge[S]],
      allPaths: mutable.ListBuffer[List[Edge[S]]]
  ): Unit = {
    visited.add(current)

    if (current == end) {
      allPaths.append(currentPath.toList)
    } else {
      for (edge <- adjacencyList.getOrElse(current, Seq())) {
        val neighbor = if (edge.nodeA == current) edge.nodeB else edge.nodeA
        if (!visited.contains(neighbor)) {
          currentPath.append(edge)
          findAllPaths(neighbor, end, visited, currentPath, allPaths)
          currentPath.remove(currentPath.length - 1)
        }
      }
    }

    visited.remove(current)
  }
}
