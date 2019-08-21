import scala.collection.mutable

object AStar {

  case class FrontierElement(loc: Location, f: Int)

  def search(src: Location, dst: Location, blocks: Set[Location]): Seq[Location] = {
    val frontier: mutable.PriorityQueue[FrontierElement] =
      mutable.PriorityQueue(FrontierElement(src, 0))(
        Ordering.by((el: FrontierElement) => el.f).reverse
      )

    var came_from: Map[Location, Location] = Map[Location, Location](src -> null)
    var cost_so_far: Map[Location, Int] = Map[Location, Int](src         -> 0)

    while (frontier.nonEmpty) {
      val top = frontier.dequeue()
      val current = top.loc
      if (current == dst) {
        return reconstruct_path(came_from, src, dst)
      } else {
        current.neighbours
          .filterNot(blocks)
          .foreach(next => {
            val new_cost = cost_so_far(current) + cost(current, next)
            if (new_cost < cost_so_far.getOrElse(next, Int.MaxValue)) {
              cost_so_far += next -> new_cost
              val priority = new_cost + heuristic(next, dst)
              frontier.enqueue(FrontierElement(next, priority))
              came_from += next -> current
            }
          })
      }
    }
    Seq.empty[Location]
  }

  def cost(src: Location, dst: Location): Int = 1

  def heuristic(src: Location, dst: Location): Int = {
    val dx = src.x - dst.x
    val dy = src.y - dst.y
    math.sqrt(dx * dx + dy * dy).toInt
  }

  def reconstruct_path(
    came_from: Map[Location, Location],
    src: Location,
    dst: Location
  ): Seq[Location] = {
    var path = List.empty[Location]
    var current = dst

    while (current != src) {
      path = current +: path
      current = came_from(current)
    }

    src :: path
  }
}
