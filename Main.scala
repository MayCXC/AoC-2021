extension (x: Int) inline def toby(y: Int) = x to y by y.compareTo(x)|1

inline val day = 5

@main def aoc = SeqMacro(One,Two,Three,Four,Five)(day,1)
