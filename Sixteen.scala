object Sixteen extends Input:
{
  val binary = input
    .head
    .flatMap( c => Integer
      .parseInt(c.toString,16)
      .toBinaryString
      .prependedAll("0000")
      .takeRight(4)
    )

  sealed abstract class Packet(version: Int, id: Int)
  case class Literal(version: Int, id: Int, value: BigInt) extends Packet(version, id)
  case class Operator(version: Int, id: Int, sub: List[Packet]) extends Packet(version, id)

  def decode(s: String): List[Packet] =
    if(s.exists('1'.equals))
      val (version, id) = (
        Integer.parseInt(s.substring(0,3),2),
        Integer.parseInt(s.substring(3,6),2)
      )

      if(id == 4)
        val (init, head :: tail) = s.substring(6).grouped(5).toList.span(_.head.equals('1'))
        Literal(version, id, BigInt(init.appended(head).map(_.tail).mkString, 2)) ::
          decode(tail.mkString)

      else s.charAt(6) match
          case '0' =>
            val length = Integer.parseInt(s.substring(7,7+15), 2)
            Operator(version, id, decode(s.substring(7+15,7+15+length))) :: decode(s.substring(7+15+length))

          case '1' =>
            val number = Integer.parseInt(s.substring(7,7+11), 2)
            val (sub, rec) = decode(s.substring(7+11)).splitAt(number)
            Operator(version, id, sub) :: rec

    else Nil

  val hierarchy :: Nil = decode(binary)

  def version_sum: Packet => Int =
    case Literal(version, id, value) => version
    case Operator(version, id, sub) => version + sub.map(version_sum).sum

  println(version_sum(hierarchy))

  def evaluate: Packet => BigInt =
    case Literal(_, _, value) => value
    case Operator(_, 0, sub) => sub.map(evaluate).sum
    case Operator(_, 1, sub) => sub.map(evaluate).product
    case Operator(_, 2, sub) => sub.map(evaluate).min
    case Operator(_, 3, sub) => sub.map(evaluate).max
    case Operator(_, 5, first :: second :: Nil) => (evaluate(first) > evaluate(second)).compareTo(false)
    case Operator(_, 6, first :: second :: Nil) => (evaluate(first) < evaluate(second)).compareTo(false)
    case Operator(_, 7, first :: second :: Nil) => (evaluate(first) == evaluate(second)).compareTo(false)
    case _ => 0

  println(evaluate(hierarchy))
}
