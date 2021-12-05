# AoC-2021

Using Scala 3 with lots of pattern matching.

Just using the standard library, `sbt ~run`, and an editor makes other languages feel archaic. The @main method uses a macro to compile one solution at a time. I only added one helper method to the standard library, for making a Range with `x to y` in the case that x>y. This already worked with an explicit call of `by -1`, so it can be added with a [simple inline extension](https://github.com/mayhd3/AoC-2021/blob/cc377f61d5784726d7925b9c1785d0f34940e0a3/Main.scala#L1). So far I only had to use [one explicit type annotation](https://github.com/mayhd3/AoC-2021/blob/5ccc832c823ecdd98ca5589f9befb7b9f3ca792b/Three.scala#L13) outside of method definitions, and I'm 90% sure overriding `filter` could fix that. 
