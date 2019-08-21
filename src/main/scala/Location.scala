case class Location(x: Int, y: Int) {

  def neighbours: Seq[Location] =
    for {
      i <- -1 to 1
      j <- -1 to 1
      if !(i == 0 && j == 0)
    } yield Location(x + i, y + j)
}
