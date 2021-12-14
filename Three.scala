object Three extends Input:
{
  val diffs = input.transpose.map(_.map{ case '0' => -1; case '1' => 1 }.sum)

  val gam = diffs.map(_ >= 0).map(_.compare(false)).reduceLeft(_<<1|_)
  val eps = diffs.map(_ <= 0).map(_.compare(false)).reduceLeft(_<<1|_)

  println(gam*eps)

  def rating(criteria: Map[Char,Seq[String]] => (Char,Seq[String]))(diagnostics: Seq[String]): String =
    if(diagnostics.isEmpty) "" else
      val (k,v) = criteria(diagnostics.groupMap(_.head)(_.tail))
      k + rating(criteria)(v.filterNot(_.isEmpty))

  def criteria(k: Char, v: Seq[String]) = (v.length,k.equals('1'))

  val oxy = Integer.parseInt(rating(_.maxBy(criteria))(input),2)
  val car = Integer.parseInt(rating(_.minBy(criteria))(input),2)

  println(oxy*car)
}
