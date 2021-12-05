import scala.collection.View

object Three extends Input:
{
  val diffs = input.transpose.map(_.map{ case '0' => -1; case '1' => 1 }.sum)

  val gam = Integer.parseInt(diffs.map(_ >= 0).map(_.compare(false)).mkString(""), 2)
  val eps = Integer.parseInt(diffs.map(_ <= 0).map(_.compare(false)).mkString(""), 2)

  println(gam * eps)

  val ox :: co :: Nil = for (criteria <- true :: false :: Nil) yield {
    var v: View[String] = input.view
    val p = diffs.indices.iterator
    while (v.tail.nonEmpty && p.hasNext) {
      val x = p.next()
      val n = v.map(_ (x)).map { case '0' => -1; case '1' => 1 }.sum
      v = v.filter(_ (x).equals(if (n < 0 == criteria) '0' else '1'))
    }
    Integer.parseInt(v.head, 2)
  }

  println(ox * co)
}
