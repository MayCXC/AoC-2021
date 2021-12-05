import scala.io.{BufferedSource, Source}

class Input:
  val file: BufferedSource = Source
    .fromFile("./input/" + this.getClass.getSimpleName.split('$').head + ".txt")
  val input: Seq[String] = file.getLines.toSeq
  file.close()
