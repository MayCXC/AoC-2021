import scala.io.{BufferedSource, Source}
import scala.util.{Success, Using}

class Input:
  val input = Using(
    Source.fromFile("./input/" + this.getClass.getSimpleName.split('$').head + ".txt")
  )(_.getLines().toSeq).getOrElse(Seq.empty)