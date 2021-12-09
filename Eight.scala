object Eight extends Input:
{
  val (signals, outputs) = input.map{
    case s"$signal | $output" => (signal.split(" "), output.split(" "))
  }.unzip

  println(outputs.map(_.map(_.size).count(Seq(2,4,3,7).contains)).sum)

  def unmixed = Array("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")

  def decodeWires(signal: Array[String]): Char => Char =
    def encode(arr: Array[String]): Char => Int =
      c => arr.filter(_.contains(c)).map(_.length^1).sum

    "abcdefg".groupBy(encode(signal)).map(_.swap).compose(_.toString)
      .andThen("abcdefg".groupBy(encode(unmixed)).andThen(_.head))
/*
  def decodeDigits(s: Array[String]): Map[Set[Char],Int] = {
    val lens = s.map(_.toSet).groupBy(_.size)
    val Seq(one, four, seven, eight) = Seq(2,4,3,7).flatMap(lens.apply)

    List(
      lens(6).find(eight.diff(four).union(one).subsetOf),
      Some(one),
      lens(5).find(eight.diff(four).subsetOf),
      lens(5).find(one.subsetOf),
      Some(four),
      lens(5).find(four.diff(one).subsetOf),
      lens(6).find(eight.diff(seven).subsetOf),
      Some(seven),
      Some(eight),
      lens(6).find(four.subsetOf)
    ).flatten.zipWithIndex.toMap
  }
*/
  def decode(signal: Array[String], output: Array[String]): String = {
    output.map(_.map(decodeWires(signal)).toSet).map(unmixed.map(_.toSet).zipWithIndex.toMap).mkString
//    output.map(decodeDigits(signal).compose(_.toSet)).mkString
  }

  println(signals.zip(outputs).map(decode.tupled).map(_.toInt).sum)
}
