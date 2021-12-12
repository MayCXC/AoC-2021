object Twelve extends Input:
{
  val paths = input.map{
    case s"$l-$r" => (l,r)
  }.flatMap((x,y) => (x,y) :: (y,x) :: Nil).groupMap(_._1)(_._2)

  def walk(small: Set[String], twice: Boolean, path: List[String]): Seq[List[String]] =
    if(path.head.equals("end"))
      Seq(path)
    else if(path.head.equals(path.head.toUpperCase))
      paths(path.head).flatMap(p => walk(small, twice, p :: path))
    else if(!small(path.head))
      paths(path.head).flatMap(p => walk(small.incl(path.head), twice, p :: path))
    else if(!path.head.equals("start") && !twice)
      paths(path.head).flatMap(p => walk(small.incl(path.head), true, p :: path))
    else Seq.empty

  println(walk(Set.empty,true,"start" :: Nil).length)

  println(walk(Set.empty,false,"start" :: Nil).length)
}
