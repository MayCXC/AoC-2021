ThisBuild / scalaVersion := "3.1.0"
run / fork := true
run / javaOptions += "-XX:+UseG1GC"
