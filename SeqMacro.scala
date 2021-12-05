import scala.quoted.{Expr, Quotes, Varargs}

object SeqMacro:
  inline def apply(inline s: Any*)(inline n: Int, inline o: Int): Any =
    ${this ('s)('n, 'o)}

  inline def apply(inline s: Any*)(inline n: Int): Any =
    apply(s *)(n, 0)

  def apply[T](s: Expr[Seq[Any]])(n: Expr[Int], o: Expr[Int])(using Quotes): Expr[Any] =
    (s, n, o) match
      case (Varargs(args), Expr(t), Expr(o)) => args(t - o)
