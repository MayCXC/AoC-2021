# AoC-2021

Using Scala 3 with lots of pattern matching. So far I have not had to use any type annotations outside of method signatures, import statements in problem objects, `while` loops, `var` declarations, or mutable datastructures.


My setup is just the standard library, a text editor with autocomplete, and a terminal with `sbt ~run`. The `@main` method uses a macro to only compile one solution at a time. So far I only added one helper method, to make a `Range` like `x to y` in the case that x>y. This can already be done with an explicit call of `by -1`, so it can be added with a [simple inline extension](https://github.com/mayhd3/AoC-2021/blob/master/Main.scala#L1).
