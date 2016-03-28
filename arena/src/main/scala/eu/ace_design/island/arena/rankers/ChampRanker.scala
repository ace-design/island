package eu.ace_design.island.arena.rankers

import eu.ace_design.island.arena.utils.{Result, OK}
import eu.ace_design.island.map.resources.{PrimaryResource, Resource}


class ChampRanker(private val _objectives: Set[(Resource, Int)]) {

  import ChampRanker._

  val objectives = _objectives.toMap

  def apply(data: Iterable[Result]): Seq[Rank] = {
    val result = filterKO(data) map { ok =>
      val fs = fulfilled(ok.resources)
      new Rank(ok.name, fulfilled2points(fs), fs, ok.remaining)
    }
    sort(result)
  }

  def fulfilled(collected: Set[(Resource, Int)]): Set[Resource] = {
    (collected filter { case (r,n) =>
      objectives.get(r) match {
        case None => false
        case Some(expected) => n >= expected
      }
    }) map { o => o._1 }
  }

  private def sort(data: Iterable[Rank]): Seq[Rank] = {
    // Ordering rule
    val byContracts = Ordering.by { data: Rank => data.points }
    val byBudget    = Ordering.by { data: Rank => data.budget }
    // Rule composition: byContracts first, then byBudget
    val complete = Ordering.by { d: Rank => (d,d) }(Ordering.Tuple2(byContracts, byBudget))
    // Applying order relation
    data.toSeq.sortWith(complete.compare(_,_) > 0 )
  }
}


object ChampRanker {

  final val PRIMARY_POINT = 1
  final val TRANSFORMED_POINT = 2

  def resource2points(r: Resource): Int = if(r.isInstanceOf[PrimaryResource]) 1 else 2

  def fulfilled2points(fulfilled: Set[Resource]): Int =  (fulfilled.toSeq map resource2points).sum

  private def filterKO(data: Iterable[Result]): Iterable[OK] = {
    (data partition { r => r.isInstanceOf[OK] })._1 map { _.asInstanceOf[OK] }
  }

}

case class Rank(name: String, points: Int, contracts: Set[Resource], budget: Int)


