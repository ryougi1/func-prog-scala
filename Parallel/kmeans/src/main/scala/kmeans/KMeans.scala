package kmeans

import org.scalameter._

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.{Map, Seq, mutable}
import scala.util.Random

class KMeans extends KMeansInterface {

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to(mutable.ArrayBuffer)
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to(mutable.ArrayBuffer)
  }

  def findClosest(p: Point, means: IterableOnce[Point]): Point = {
    val it = means.iterator
    assert(it.nonEmpty)
    var closest = it.next()
    var minDistance = p.squareDistance(closest)
    while (it.hasNext) {
      val point = it.next()
      val distance = p.squareDistance(point)
      if (distance < minDistance) {
        minDistance = distance
        closest = point
      }
    }
    closest
  }

  /**
    * Take a sequence of points and a sequence of means, and return a map
    * collection, which maps each mean to the sequence of points in the
    * corresponding cluster.
    *
    * Use groupBy and the findClosest method, which is already defined for
    * you. After that, make sure that all the means are in the resulting map,
    * even if their sequences are empty.
    * def groupBy[K](f: (A) => K): immutable.Map[K, Seq[A]]
    */
  def classify(points: Seq[Point], means: Seq[Point]): Map[Point, Seq[Point]] = {
    if (points.isEmpty) means.map(point => (point, Seq.empty[Point])).toMap
    else points.groupBy(point => findClosest(point, means))
  }

  def classify(points: ParSeq[Point], means: ParSeq[Point]): ParMap[Point, ParSeq[Point]] = {
    if (points.isEmpty) means.map(point => (point, ParSeq.empty[Point])).toMap
    else points.groupBy(point => findClosest(point, means))
  }

  def findAverage(oldMean: Point, points: Seq[Point]): Point = if (points.isEmpty) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def findAverage(oldMean: Point, points: ParSeq[Point]): Point = if (points.isEmpty) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  /**
    * Takes the map of classified points produced in the previous step, and
    * the sequence of previous means. The method returns the new sequence of
    * means.
    *
    * Take care to preserve order in the resulting sequence -- the mean i in
    * the resulting sequence must correspond to the mean i from oldMeans.
    *
    * Make sure you use the findAverage method that is predefined for you.
    * def findAverage(oldMean: Point, points: Seq[Point]): Point
    */
  def update(classified: Map[Point, Seq[Point]], oldMeans: Seq[Point]): Seq[Point] = {
    oldMeans.map((oldMean) => findAverage(oldMean, classified(oldMean)))
  }

  def update(classified: ParMap[Point, ParSeq[Point]], oldMeans: ParSeq[Point]): ParSeq[Point] = {
    oldMeans.map((oldMean) => findAverage(oldMean, classified(oldMean)))
  }

  /**
    * Takes a sequence of old means and the sequence of updated means, and
    * returns a boolean indicating if the algorithm converged or not.
    */
  def converged(eta: Double, oldMeans: Seq[Point], newMeans: Seq[Point]): Boolean = {
    oldMeans.zip(newMeans).forall {
      case (newMean, oldMean) =>
        oldMean.squareDistance(newMean) <= eta
    }
  }

  def converged(eta: Double, oldMeans: ParSeq[Point], newMeans: ParSeq[Point]): Boolean = {
    oldMeans.zip(newMeans).forall {
      case (newMean, oldMean) =>
        oldMean.squareDistance(newMean) <= eta
    }
  }

  /**
    * The tail-recursive kMeans method takes a sequence of points, previously
    * computed sequence of means, and the eta value, and return the sequence
    * of means, each corresponding to a specific cluster.
    */
  @tailrec
  final def kMeans(points: Seq[Point], means: Seq[Point], eta: Double): Seq[Point] = {
    val classified = classify(points, means)
    val updated = update(classified, means)
    if (!converged(eta, means, updated)) kMeans(points, updated, eta)
    else updated
  }

  @tailrec
  final def kMeans(points: ParSeq[Point], means: ParSeq[Point], eta: Double): ParSeq[Point] = {
    val classified = classify(points, means)
    val updated = update(classified, means)
    if (!converged(eta, means, updated)) kMeans(points, updated, eta)
    else updated
  }
}

/** Describes one point in three-dimensional space.
  *
  * Note: deliberately uses reference equality.
  */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v

  def squareDistance(that: Point): Double = {
    square(that.x - x) + square(that.y - y) + square(that.z - z)
  }

  private def round(v: Double): Double = (v * 100).toInt / 100.0

  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }

    val parPoints = points.par
    val parMeans = means.par

    val partime = standardConfig measure {
      kMeans.kMeans(parPoints, parMeans, eta)
    }

    // Additional `println` to avoid bad interaction with JLine output
    println()
    println()
    println()
    println()
    println(s"sequential time: $seqtime")
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
    println()
    println()
    println()
  }

  // Workaround Dotty's handling of the existential type KeyValue
  implicit def keyValueCoerce[T](kv: (Key[T], T)): KeyValue = {
    kv.asInstanceOf[KeyValue]
  }
}
