package stackoverflow

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.annotation.tailrec

/** A raw stackoverflow posting, either a question or an answer */
case class Posting(postingType: Int, id: Int, acceptedAnswer: Option[Int], parentId: Option[QID], score: Int, tags: Option[String]) extends Serializable


/** The main class */
object StackOverflow extends StackOverflow {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local[*]")
    .setAppName("StackOverflow")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

  /** Main function */
  def main(args: Array[String]): Unit = {

    // the lines from the csv file as strings
    // <postTypeId>,<id>,[<acceptedAnswer>],[<parentId>],<score>,[<tag>]
    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    // the raw Posting entries for each line
    val raw = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored = scoredPostings(grouped)
    val vectors = vectorPostings(scored).persist()
    assert(vectors.count() == 2121822, "Incorrect number of vectors: " + vectors.count())
    println("Vectors counts: " + vectors.count())

    //    val means = kmeans(sampleVectors(vectors), vectors, debug = true)
    val means = kmeans(sampleVectors(vectors), vectors, debug = false)
    val results = clusterResults(means, vectors)
    printResults(results)
  }
}

/** The parsing and kmeans methods */
class StackOverflow extends StackOverflowInterface with Serializable {

  /** Languages */
  val langs =
    List(
      "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
      "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  /** K-means parameter: How "far apart" languages should be for the kmeans algorithm? */
  def langSpread = 50000

  assert(langSpread > 0, "If langSpread is zero we can't recover the language from the input data!")

  /** K-means parameter: Number of clusters */
  def kmeansKernels = 45

  /** K-means parameter: Convergence criteria */
  def kmeansEta: Double = 20.0D

  /** K-means parameter: Maximum iterations */
  def kmeansMaxIterations = 120


  //
  //
  // Parsing utilities:
  //
  //

  /** Load postings from the given file */
  def rawPostings(lines: RDD[String]): RDD[Posting] =
    lines.map(line => {
      val arr = line.split(",")
      Posting(postingType = arr(0).toInt,
        id = arr(1).toInt,
        acceptedAnswer = if (arr(2) == "") None else Some(arr(2).toInt),
        parentId = if (arr(3) == "") None else Some(arr(3).toInt),
        score = arr(4).toInt,
        tags = if (arr.length >= 6) Some(arr(5).intern()) else None)
    })


  /** Group the questions and answers together
    *
    * Match on QID, producing an RDD[(QID, Iterable[(Question, Answer))].
    * 1. Filter the questions and answers separately
    * 2. Extracting the QID value in the first element of a tuple
    * 3. Use one of the join operations (which one?) to obtain an RDD[(QID, (Question, Answer))].
    * 4. Obtain an RDD[(QID, Iterable[(Question, Answer)])]
    */
  def groupedPostings(postings: RDD[Posting]): RDD[(QID, Iterable[(Question, Answer)])] = {
    val questions: RDD[(Int, Posting)] = postings.filter(p =>
      p.postingType == 1).map(q => (q.id, q))
    val answers: RDD[(Int, Posting)] = postings.filter(p =>
      p.postingType == 2 && p.parentId.isDefined).map(a => (a.parentId.get, a))
    questions.join(answers).groupByKey()
  }


  /**
    * Compute the maximum score for each posting
    * Return an RDD containing pairs of (a) questions and (b) the score of the
    * answer with the highest score.
    * Note: this does not have to be the answer marked as acceptedAnswer!
    *
    * Hint: use the provided answerHighScore given in scoredPostings.
    */
  def scoredPostings(grouped: RDD[(QID, Iterable[(Question, Answer)])]): RDD[(Question, HighScore)] = {

    def answerHighScore(as: Array[Answer]): HighScore = {
      var highScore = 0
      var i = 0
      while (i < as.length) {
        val score = as(i).score
        if (score > highScore)
          highScore = score
        i += 1
      }
      highScore
    }

    grouped.map { case (_, qa) =>
      (qa.head._1, answerHighScore(qa.map { case (_, a) => a }.toArray))
    }
  }


  /** Compute the vectors for the kmeans
    * The vectors should be pairs with two components
    * (1) Index of the language (in the langs list) multiplied by the
    * langSpread factor.
    * (2) The highest answer score (computed above).
    */
  def vectorPostings(scored: RDD[(Question, HighScore)]): RDD[(LangIndex, HighScore)] = {
    /** Return optional index of first language that occurs in `tags`. */
    def firstLangInTag(tag: Option[String], ls: List[String]): Option[Int] = {
      if (tag.isEmpty) None
      else if (ls.isEmpty) None
      else if (tag.get == ls.head) Some(0) // index: 0
      else {
        val tmp = firstLangInTag(tag, ls.tail)
        tmp match {
          case None => None
          case Some(i) => Some(i + 1) // index i in ls.tail => index i+1
        }
      }
    }

    scored.filter { case (q, _) => q.tags != None }.map { case (q, hs) =>
      (firstLangInTag(q.tags, langs).get * langSpread, hs)
    }
  }


  /** Sample the vectors */
  def sampleVectors(vectors: RDD[(LangIndex, HighScore)]): Array[(Int, Int)] = {

    assert(kmeansKernels % langs.length == 0, "kmeansKernels should be a multiple of the number of languages studied.")
    val perLang = kmeansKernels / langs.length

    // http://en.wikipedia.org/wiki/Reservoir_sampling
    def reservoirSampling(lang: Int, iter: Iterator[Int], size: Int): Array[Int] = {
      val res = new Array[Int](size)
      val rnd = new util.Random(lang)

      for (i <- 0 until size) {
        assert(iter.hasNext, s"iterator must have at least $size elements")
        res(i) = iter.next
      }

      var i = size.toLong
      while (iter.hasNext) {
        val elt = iter.next
        val j = math.abs(rnd.nextLong) % i
        if (j < size)
          res(j.toInt) = elt
        i += 1
      }

      res
    }

    val res =
      if (langSpread < 500)
      // sample the space regardless of the language
      vectors.takeSample(false, kmeansKernels, 42)
      else
      // sample the space uniformly from each language partition
      vectors.groupByKey.flatMap({
        case (lang, vectors) => reservoirSampling(lang, vectors.toIterator, perLang).map((lang, _))
      }).collect()

    assert(res.length == kmeansKernels, res.length)
    res
  }


  //
  //
  //  Kmeans method:
  //
  //

  /** Main kmeans computation */
  @tailrec final def kmeans(means: Array[(Int, Int)], vectors: RDD[(Int, Int)], iter: Int = 1, debug: Boolean = false): Array[(Int, Int)] = {
    val newMeans = means.clone()

    // def findClosest(p: (Int, Int), centers: Array[(Int, Int)]): Int
    // def averageVectors(ps: Iterable[(Int, Int)]): (Int, Int)
    // def euclideanDistance(a1: Array[(Int, Int)], a2: Array[(Int, Int)]): Double

    // Pair each vector with the index of the closest mean (its cluster) and
    // then compute the new means by averaging the values of each cluster.

    val clusters = vectors
      // Pair into RDD[(meanIndex, (x, y))] using findClosest
      .map(coord => (findClosest(coord, means), coord))
      // Turn into RDD[(meanIndex, Iterable[(x, y)])] using groupByKey
      .groupByKey()
      // Calc avgs using averageVectors and assign them to newMeans
      //      .map{ case (meanIndex, iter) =>
      //        newMeans(meanIndex) = averageVectors(iter)
      //      }
      .mapValues(iter => averageVectors(iter))
      .collect()

    clusters.foreach {
      case (index, avg) => newMeans(index) = avg
    }

    val distance = euclideanDistance(means, newMeans)

    if (debug) {
      println(
        s"""Iteration: $iter
           |  * current distance: $distance
           |  * desired distance: $kmeansEta
           |  * means:""".stripMargin)
      for (idx <- 0 until kmeansKernels)
        println(f"   ${means(idx).toString}%20s ==> ${newMeans(idx).toString}%20s  " +
          f"  distance: ${euclideanDistance(means(idx), newMeans(idx))}%8.0f")
    }

    if (converged(distance))
      newMeans
    else if (iter < kmeansMaxIterations)
      kmeans(newMeans, vectors, iter + 1, debug)
    else {
      if (debug) {
        println("Reached max iterations!")
      }
      newMeans
    }
  }


  //
  //
  //  Kmeans utilities:
  //
  //

  /** Decide whether the kmeans clustering converged */
  def converged(distance: Double) =
    distance < kmeansEta


  /** Return the euclidean distance between two points */
  def euclideanDistance(v1: (Int, Int), v2: (Int, Int)): Double = {
    val part1 = (v1._1 - v2._1).toDouble * (v1._1 - v2._1)
    val part2 = (v1._2 - v2._2).toDouble * (v1._2 - v2._2)
    part1 + part2
  }

  /** Return the euclidean distance between two points */
  def euclideanDistance(a1: Array[(Int, Int)], a2: Array[(Int, Int)]): Double = {
    assert(a1.length == a2.length)
    var sum = 0d
    var idx = 0
    while (idx < a1.length) {
      sum += euclideanDistance(a1(idx), a2(idx))
      idx += 1
    }
    sum
  }

  /** Return the closest point */
  def findClosest(p: (Int, Int), centers: Array[(Int, Int)]): Int = {
    var bestIndex = 0
    var closest = Double.PositiveInfinity
    for (i <- 0 until centers.length) {
      val tempDist = euclideanDistance(p, centers(i))
      if (tempDist < closest) {
        closest = tempDist
        bestIndex = i
      }
    }
    bestIndex
  }


  /** Average the vectors */
  def averageVectors(ps: Iterable[(Int, Int)]): (Int, Int) = {
    val iter = ps.iterator
    var count = 0
    var comp1: Long = 0
    var comp2: Long = 0
    while (iter.hasNext) {
      val item = iter.next
      comp1 += item._1
      comp2 += item._2
      count += 1
    }
    ((comp1 / count).toInt, (comp2 / count).toInt)
  }


  //
  //
  //  Displaying results:
  //
  //
  def clusterResults(means: Array[(Int, Int)], vectors: RDD[(LangIndex, HighScore)]): Array[(String, Double, Int, Int)] = {
    val closest = vectors.map(p => (findClosest(p, means), p))
    val closestGrouped = closest.groupByKey()

    /**
      * For each cluster, compute:
      *
      * (a) the dominant programming language in the cluster;
      * (b) the percent of answers that belong to the dominant language;
      * (c) the size of the cluster (the number of questions it contains);
      * (d) the median of the highest answer scores.
      */
    // RDD[Int, Iterable[(LangIndex, Highscore)]]
    val median = closestGrouped.mapValues { vs =>
      val langOccurrences =
        vs.map {
          case (langIndex, highScore) => langIndex / langSpread
        } // Iterable[index]
          .groupBy(identity) // Map[index, Iterable[index]]
          .mapValues(_.size) // Map[index, nrOccurrences]
      val dominantIndex = langOccurrences.maxBy(_._2)._1

      val dominantLang: String = langs(dominantIndex)
      val dominantLangPerc: Double =
        langOccurrences(dominantIndex).toDouble / vs.size.toDouble * 100.0
      val clusterSize: Int = vs.size

      val sorted = vs.map(_._2).toList.sorted
      val medianHighscore: Int =
        if (clusterSize % 2 == 0)
          (sorted(clusterSize / 2 - 1) + sorted(clusterSize / 2)) / 2
        else sorted(clusterSize / 2)

      (dominantLang, dominantLangPerc, clusterSize, medianHighscore)
    }

    median.collect().map(_._2).sortBy(_._4)
  }

  def printResults(results: Array[(String, Double, Int, Int)]): Unit = {
    println("Resulting clusters:")
    println("  Score  Dominant language (%percent)  Questions")
    println("================================================")
    for ((lang, percent, size, score) <- results)
      println(f"${score}%7d  ${lang}%-17s (${percent}%-5.1f%%)      ${size}%7d")
  }
}
