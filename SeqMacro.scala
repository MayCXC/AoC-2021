import scala.quoted.{Expr, Quotes, Varargs}

object SeqMacro:
  inline def apply(inline n: Int, inline o: Int)(inline s: Any*): Any =
    ${this('n, 'o)('s)}

  inline def apply(inline n: Int)(inline s: Any*): Any =
    apply(n, 0)(s*)

  def apply[T](n: Expr[Int], o: Expr[Int])(s: Expr[Seq[Any]])(using Quotes): Expr[Any] =
    (s, n, o) match
      case (Varargs(args), Expr(t), Expr(o)) => args(t - o)
