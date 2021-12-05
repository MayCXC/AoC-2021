object Four extends Input {{
    val (draws, boards) = input match
      case Seq(head, "", tail*) => (
        head.split(",").map(_.toInt).toSeq,
        tail.indices
          .filter(tail(_).equals(""))
          .prepended(0)
          .appended(tail.length)
          .sliding(2)
          .map { case Seq(l, r) => (l + 1 until r)
            .map(tail)
            .map(_.trim.split("\\s+").map(_.toInt).toSeq)
          }.toSeq
      )

    def findScore(draw: Seq[Int], board: Seq[Seq[Int]]): Option[Int] = Option.when(
      (board :: board.transpose :: Nil).exists(_.exists(_.toSet.subsetOf(draw.toSet)))
    )(board.flatten.filterNot(draw.toSet).sum)

    val winner #:: _ = for
      draw <- draws.inits.to(LazyList).reverse
      board <- boards
      score <- findScore(draw, board)
    yield score * draw.last

    println(winner)

    val loser #:: _ = for
      draw <- draws.inits.to(LazyList)
      board <- boards
      score <- Option.when(findScore(draw.init, board).isEmpty)(draw, board)
        .flatMap(findScore)
    yield score * draw.last

    println(loser)
  }
}
