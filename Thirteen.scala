object Thirteen extends Input:
{
  val (dots, _ :: inst) = input.span(_.nonEmpty)

  val paper = dots.map{case s"$l,$r" => (l.toInt,r.toInt)}.toSet

  val instructions = inst.map{
    case s"fold along x=$x" => (x.toInt,0)
    case s"fold along y=$y" => (0,y.toInt)
  }

  def reflect(set: Set[(Int,Int)]): List[(Int,Int)] => Set[(Int,Int)] =
    case Nil => set
    case head :: tail => reflect(
      set.map((x,y) => (
          if(head._1 == 0 || x < head._1) x else 2*head._1 - x,
          if(head._2 == 0 || y < head._2) y else 2*head._2 -y
        )
      )
    )(tail)

  println(reflect(paper)(instructions.head :: Nil).size)

  val code = reflect(paper)(instructions)
  val (width, height) = (code.map(_._1).max + 1, code.map(_._2).max + 1)

  val screen = Array.tabulate(height,width)((y,x) => if(code((x,y))) '#' else '.')

  println(screen.map(_.mkString).mkString("\n"))
}
