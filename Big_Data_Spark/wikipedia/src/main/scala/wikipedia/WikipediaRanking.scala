package wikipedia

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

case class WikipediaArticle(title: String, text: String) {
  /**
    * @return Whether the text of this article mentions `lang` or not
    * @param lang Language to look for (e.g. "Scala")
    */
  def mentionsLanguage(lang: String): Boolean = text.split(' ').contains(lang)
}

object WikipediaRanking extends WikipediaRankingInterface {

  val langs = List(
    "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
    "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  val conf: SparkConf = new SparkConf().setMaster("local[*]")
    .setAppName("wikipedia")
  val sc: SparkContext = new SparkContext(conf)
  /**
    * There are several ways to read data into Spark. The simplest way to read
    * in data is to convert an existing collection in memory to an RDD using
    * the parallelize method of the Spark context.
    *
    * We have already implemented a method parse in the object WikipediaData
    * object that parses a line of the dataset and turns it into a
    * WikipediaArticle.
    *
    * Hint: use a combination of `sc.parallelize`, `WikipediaData.lines`
    * and `WikipediaData.parse`
    */
  val wikiRdd: RDD[WikipediaArticle] = sc.parallelize(WikipediaData.lines.map
  (line => WikipediaData.parse(line)))

  /** Returns the number of articles on which the language `lang` occurs.
    * Hint1: consider using method `aggregate` on RDD[T].
    * def aggregate[U](zeroValue: U)(seqOp: (U, T) ⇒ U, combOp: (U, U) ⇒ U): U
    * Hint2: consider using method `mentionsLanguage` on `WikipediaArticle`
    * def mentionsLanguage(lang: String): Boolean =
    */
  def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int =
    rdd.aggregate(0)((acc, article) =>
      if (article.mentionsLanguage(lang)) acc + 1
      else acc,
      _ + _
    )

  /** (1) Use `occurrencesOfLang` to compute the ranking of the languages
    * (`val langs`) by determining the number of Wikipedia articles that
    * mention each language at least once. Don't forget to sort the
    * languages by their occurrence, in decreasing order!
    *
    * Note: this operation is long-running. It can potentially run for
    * several seconds.
    */
  def rankLangs(langs: List[String], rdd: RDD[WikipediaArticle]):
  List[(String, Int)] =
    langs.zip(langs.map(lang => occurrencesOfLang(lang, rdd)))
      .sortBy { case (_, count) => count }.reverse

  /** Compute an inverted index of the set of articles, mapping each language
    * to the Wikipedia pages in which it occurs.
    *
    * Hint: You might want to use methods flatMap and groupByKey on RDD for
    * this part.
    * def flatMap[B](f: A => TraversableOnce[B]): RDD[B]
    * groupByKey: When called on a dataset of (K, V) pairs, returns a dataset
    * of (K, Iterable<V>) pairs.
    */
  def makeIndex(langs: List[String], rdd: RDD[WikipediaArticle]):
  RDD[(String, Iterable[WikipediaArticle])] =
    rdd.flatMap(article =>
      langs.filter(lang => article.mentionsLanguage(lang))
        .map(lang => (lang, article))
    ).groupByKey()

  /** (2) Compute the language ranking again, but now using the inverted
    * index. Can you notice a performance improvement?
    *
    * Note: this operation is long-running. It can potentially run for
    * several seconds.
    *
    * Hint: method mapValues on PairRDD could be useful for this part.
    */
  def rankLangsUsingIndex(index: RDD[(String, Iterable[WikipediaArticle])]): List[(String, Int)] =
    index.mapValues(articles => articles.size).collect().toList
      .sortBy { case (_, count) => count }.reverse

  /** (3) Use `reduceByKey` so that the computation of the index and the
    * ranking are combined.
    * Can you notice an improvement in performance compared to measuring *both* the computation of the index
    * and the computation of the ranking? If so, can you think of a reason?
    *
    * Note: this operation is long-running. It can potentially run for
    * several seconds.
    */
  def rankLangsReduceByKey(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =
    rdd.flatMap(article =>
      langs.filter(lang => article.mentionsLanguage(lang))
        .map(lang => (lang, 1))
    ).reduceByKey(_ + _).collect().toList.sortBy { case (_, count) => count }
      .reverse

  def main(args: Array[String]): Unit = {

    /* Languages ranked according to (1) */
    val langsRanked: List[(String, Int)] = timed("Part 1: naive ranking", rankLangs(langs, wikiRdd))
    println(langsRanked.take(10))

    /* An inverted index mapping languages to wikipedia pages on which they appear */
    def index: RDD[(String, Iterable[WikipediaArticle])] = makeIndex(langs, wikiRdd)

    /* Languages ranked according to (2), using the inverted index */
    val langsRanked2: List[(String, Int)] = timed("Part 2: ranking using inverted index", rankLangsUsingIndex(index))
    println(langsRanked2.take(10))

    /* Languages ranked according to (3) */
    val langsRanked3: List[(String, Int)] = timed("Part 3: ranking using reduceByKey", rankLangsReduceByKey(langs, wikiRdd))
    println(langsRanked3.take(10))

    /* Output the speed of each ranking */
    println(timing)
    sc.stop()
  }

  val timing = new StringBuffer

  def timed[T](label: String, code: => T): T = {
    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")
    result
  }
}
