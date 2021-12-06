# AoC-2021

Using Scala 3 with lots of pattern matching.

My setup is just the standard library, a text editor with autocomplete, and a terminal with `sbt ~run`. The @main method uses a macro to only compile one solution at a time. So far I only added one helper method, to make a `Range` like `x to y` in the case that x>y. This can already be done with an explicit call of `by -1`, so it can be added with a [simple inline extension](https://github.com/mayhd3/AoC-2021/blob/cc377f61d5784726d7925b9c1785d0f34940e0a3/Main.scala#L1). I have not had to use any type annotations outside of method parameters, `while` loops, or `var`.
