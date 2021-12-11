object Three extends Input:
{
  val diffs = input.transpose.map(_.map{ case '0' => -1; case '1' => 1 }.sum)

  val gam = Integer.parseInt(diffs.map(_ >= 0).map(_.compare(false)).mkString(""), 2)
  val eps = Integer.parseInt(diffs.map(_ <= 0).map(_.compare(false)).mkString(""), 2)

  println(gam*eps)

  def rating(criteria: Map[Char,Seq[String]] => (Char,Seq[String]))(diagnostics: Seq[String]): String =
    if(diagnostics.isEmpty) "" else
      val (k,v) = criteria(diagnostics.groupMap(_.head)(_.tail))
      k + rating(criteria)(v.filterNot(_.isEmpty))

  def o2criteria(k: Char, v: Seq[String]) = (v.length,k.equals('1'))

  val oxy = Integer.parseInt(rating(_.maxBy(o2criteria))(input),2)
  val car = Integer.parseInt(rating(_.minBy(o2criteria))(input),2)

  println(oxy*car)
}
