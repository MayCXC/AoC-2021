import scala.collection.mutable

object Nineteen extends Input:
{
  val reports = input.zipWithIndex.flatMap{
      case (s"--- scanner $n ---",i) => Some((n.toInt,i))
      case _ => None
    }
    .map{
      case (n,i) => n -> input.drop(i).tail.takeWhile(_.nonEmpty).map{
        case s"$x,$y,$z" => (x.toInt, y.toInt, z.toInt)
      }.toSet
    }.toMap

  def map3[L,R](f: L => R): ((L,L,L)) => (R,R,R) = (x,y,z) => (f(x),f(y),f(z))

  def sum: ((Int,Int,Int)) => Int = _+_+_

  def manhattan: ((Int,Int,Int),(Int,Int,Int)) => Int =
    (l,r) => map3[(Int,Int),Int]( (x,y) => (Math.max(x,y)-Math.min(x,y))).andThen(sum)( l.zip(r) )

  def mean: ((Int,Int)) => Int = (left,right) =>
    (left + right)>>1

  case class Beacon(
    position: (Int,Int,Int),
    neighbors: Set[Int]
  )

  case class Region(
    scanners: Set[(Int,Int,Int)],
    detected: Set[Beacon],
    mad: Int
  )

  object Region{
    def apply(scanners: Set[(Int,Int,Int)], beacons: Set[(Int,Int,Int)]): Region = Region(
      scanners,
      beacons.map( beacon =>
        Beacon(
          beacon,
          beacons.excl(beacon).map(manhattan(beacon,_))
        )
      ),
      map3[(Int,Set[Int]),Int]
        ((m,s) => mean(median(s.map(_-m).map(_.abs)))).andThen(sum)
      (map3(median[Int].andThen(mean))(beacons.unzip3).zip(beacons.unzip3))
    )
  }

  val scanners: Map[Int,Region] =
    reports.map((n, beacons) => n -> Region(Set((0,0,0)), beacons))

  def orientations = List[(Int,Int,Int) => (Int,Int,Int)](
    (x,y,z) => (+x,+y,+z), (x,y,z) => (-x,+y,-z), (x,y,z) => (+x,-y,-z), (x,y,z) => (-x,-y,+z),
    (x,y,z) => (+x,+z,-y), (x,y,z) => (-x,+z,+y), (x,y,z) => (+x,-z,+y), (x,y,z) => (-x,-z,-y),
    (x,y,z) => (+y,+x,-z), (x,y,z) => (-y,+x,+z), (x,y,z) => (+y,-x,+z), (x,y,z) => (-y,-x,-z),
    (x,y,z) => (+y,+z,+x), (x,y,z) => (-y,+z,-x), (x,y,z) => (+y,-z,-x), (x,y,z) => (-y,-z,+x),
    (x,y,z) => (+z,+x,+y), (x,y,z) => (-z,+x,-y), (x,y,z) => (+z,-x,-y), (x,y,z) => (-z,-x,+y),
    (x,y,z) => (+z,+y,-x), (x,y,z) => (-z,+y,+x), (x,y,z) => (+z,-y,+x), (x,y,z) => (-z,-y,-x)
  )

  def fit(left: Region, right: Region): Option[Region] = {
    val pairs = for(
      l <- left.detected;
      r <- right.detected
    ) yield (l,r)

    for((l,r) <- pairs.toSeq.sortBy((l,r) => l.neighbors.intersect(r.neighbors).size)) {
      val lt = left.detected.map{case Beacon(p,n) => Beacon(map3[(Int,Int),Int](_-_)(p.zip(l.position)),n)}
      val rt = right.detected.map{case Beacon(p,n) => Beacon(map3[(Int,Int),Int](_-_)(p.zip(r.position)),n)}
      for(orientation <- orientations) {
        val ro = rt.map{ case Beacon(p,n) => Beacon(orientation.tupled(p),n) }
        if(lt.map(_.position).intersect(ro.map(_.position)).size >= 12) {
          val lb = left.scanners.map(p => map3[(Int,Int),Int](_-_)(p.zip(l.position)))
          val rb = right.scanners.map(p => map3[(Int,Int),Int](_-_)(p.zip(r.position))).map(orientation.tupled)
          return Some( Region(
            lb union rb,
            (lt union ro)
              .groupMapReduce(_.position)(_.neighbors)(_.union(_))
              .toSet
              .map(Beacon.apply),
            mean(left.mad, right.mad)
          ) )
        }
      }
    }
    None
  }

  println(fit(scanners(0),scanners(1)))

  val full = mutable.ArrayDeque.from(scanners.values)

  def fitNext(): Region = {
    val fail = full.empty
    val head = full.removeHead()
    while(full.nonEmpty) {
      val next = full.removeHead()
      val test = fit(head,next)
      if(test.isDefined) {
        full.prependAll(fail)
        full.appendAll(test)
        return full.head
      }
      else {
        fail.prepend(next)
      }
    }
    full.prependAll(fail)
    full.append(head)
    return head
  }

  while(full.tail.nonEmpty) {   // todo make sortby a heap again and use closest mad
    println("fitting " + full.length)
    fitNext()
  }

  val fullmap = full.head
  println(fullmap.detected.size)

  println(fullmap.scanners.toSeq.combinations(2).map{case Seq(l,r) => (l,r)}.map(manhattan.tupled).max)
}
