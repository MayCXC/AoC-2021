extension (x: Int) inline def toby(inline y: Int) = x to y by y.compareTo(x)|1

inline def day = 5
@main def aoc =
  SeqMacro(day,1)
    (One,Two,Three,Four,Five)
