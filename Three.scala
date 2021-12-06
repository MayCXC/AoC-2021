object Three extends Input:
{
  val diffs = input.transpose.map(_.map{ case '0' => -1; case '1' => 1 }.sum)

  val gam = Integer.parseInt(diffs.map(_ >= 0).map(_.compare(false)).mkString(""), 2)
  val eps = Integer.parseInt(diffs.map(_ <= 0).map(_.compare(false)).mkString(""), 2)

  println(gam*eps)

  def rating(sign: Int, tie: Char): Seq[String] => String =
    case Seq() => ""
    case diags =>
      val (k,v) = diags.groupMap(_.head)(_.tail).maxBy((k,v) => (v.length*sign,k.equals(tie)))
      k + rating(sign,tie)(v.filterNot("".equals))

  val oxygen = Integer.parseInt(rating(1,'1')(input),2)
  val carbon = Integer.parseInt(rating(-1,'0')(input),2)

  println(oxygen*carbon)
}
