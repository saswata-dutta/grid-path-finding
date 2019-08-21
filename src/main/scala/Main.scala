object Main {

  def main(args: Array[String]): Unit = {
    val src = Location(5, 1)
    val dst = Location(1, 5)
    val blocks = Set(Location(2, 2), Location(2, 3), Location(2, 4), Location(3, 4), Location(4, 4))

    val path = AStar.search(src, dst, blocks)
    path.foreach(println)
  }
}
