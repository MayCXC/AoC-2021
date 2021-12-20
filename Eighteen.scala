object Eighteen extends Input:
{
  def transform(d: Int): List[Char] => List[(Int,Int)] = {
    case '[' :: tail => transform(d+1)(tail)
    case ']' :: tail => transform(d-1)(tail)
    case ',' :: tail => transform(d)(tail)
    case Nil => Nil
    case element =>
      val (el,tail) = element.span("0123456789".toSet)
      (d,el.mkString.toInt) :: transform(d)(tail)
  }

  def transform: List[Char] => List[(Int,Int)] = transform(0)(_)

  def explode(list: List[(Int,Int)], index: Int): List[(Int,Int)] =
    val List(init, prev, List((ld,lv)), List((rd,rv)), next, tail) =
      Seq(0, index-1, index, index+1, index+2, index+3, list.length)
        .sliding(2)
        .map{case Seq(l, r) => list.slice(l,r)}
        .toList

    List(
      init,
      prev.map{case (dep,reg) => (dep,reg+lv)},
      List((((ld+rd)>>1)-1,0)),
      next.map{case (dep,reg) => (dep,reg+rv)},
      tail
    ).flatten

  def split(list: List[(Int,Int)], index: Int): List[(Int,Int)] =
    val List(init, List((d,v)), tail) =
      Seq(0,index,index+1,list.length)
        .sliding(2)
        .map{case Seq(l,r) => list.slice(l,r)}
        .toList

    List(
      init,
      List((d+1,v>>1),(d+1,(v>>1) + (v&1))),
      tail
    ).flatten

  def reduce(list: List[(Int,Int)]): List[(Int,Int)] =
    val explodeAt = list.indexWhere(_._1.equals(5))
    if(explodeAt >= 0)
      return reduce(explode(list,explodeAt))

    val splitAt = list.indexWhere(_._2 >= 10)
    if(splitAt >= 0)
      return reduce(split(list,splitAt))

    list

  def addition(addend: List[(Int,Int)], augend: List[(Int,Int)]): List[(Int,Int)] =
    addend.map{case (d,v) => (d+1,v)} ++ augend.map{case (d,v) => (d+1,v)}

  def fixedsum(addend: Set[Int], augend: Int): Set[Int] =
    if(!addend(augend)) addend.incl(augend)
    else fixedsum(addend.excl(augend), augend+1)

  def neighbor(height: Int, bits: Set[Int], in: List[(Int,Int)], out: List[(Int,Int)]): (List[(Int,Int)],List[(Int,Int)]) = {
    (Nil,Nil)
  }

  def mag: List[(Int,Int)] => Int = {
      case (d,v) :: Nil => v
      case (d1,v1) :: (d2,v2) :: tail if d1 == d2 =>
        mag((((d1+d2)>>1)-1, 3*v1 + 2*v2) :: tail)
      case (d,v) :: tail =>
        val (in, out) = tail.span(_._1 > d)
        val part = 3*v + 2*mag(in.map{case (din, vin) => (din-d,vin)})
        mag((d-1, part) :: out)
      case Nil => 0
    }

  def magnitude: List[(Int,Int)] => Int =
    case (d,v) :: Nil => v
    case list =>
      val deepest = list.maxBy(_._1)._1
      val Seq(l,r) = list.indices.filter(list(_)._1.equals(deepest)).grouped(2).next
      magnitude(list.take(l) ++ List((list(l)._1-1,3*list(l)._2 + 2*list(r)._2)) ++ list.drop(r+1))

  val numbers = input.map(_.toList).map(transform)

  println(magnitude(numbers.reduceLeft((l,r) => reduce(addition(l,r)))))

  println(numbers.combinations(2).flatMap(_.permutations).map{case l :: r :: Nil => magnitude(reduce(addition(l,r))) }.max)
  println(numbers.combinations(2).flatMap(_.permutations).map{case l :: r :: Nil => mag(reduce(addition(l,r))) }.max)
}
