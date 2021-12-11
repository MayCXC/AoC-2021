import scala.annotation.tailrec

object Ten extends Input:
{
  def legal = Set("()","[]","{}","<>")

  def transform(stack: List[Char]): List[Char] => List[Char] = {
    case head :: tail if "([{<".contains(head) => transform(head :: stack)(tail)
    case head :: tail if ">}])".contains(head) => stack.head :: head :: transform(stack.tail)(tail)
    case head :: tail => head :: transform(stack)(tail)
    case Nil => '_' :: stack
  }

  val (incomplete, corrupted) = input
    .map(transform(Nil).compose(_.toList))
    .map(_.mkString.split("_"))
    .partition(_.head.grouped(2).map(_.mkString).forall(legal))

  println(
    corrupted
      .flatMap(_.head.grouped(2).map(_.mkString).find(legal.andThen(!_)))
      .map(_.tail.head)
      .map(Map(')'->3, ']'->57, '}'->1197, '>'->25137))
      .sum
  )

  val autocomplete = incomplete
    .map(_.tail.head.map(Map('('->1, '['->2, '{'->3, '<'->4)).foldLeft(BigInt("0"))((l,r) => l*5+r))
    .sorted

//  println(autocomplete(autocomplete.length/2))

  println(median(autocomplete)._1)
}
