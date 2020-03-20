package reductions

import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def compareParenthesis(nrOpen: Int, chars: Array[Char]): Boolean =
      if (chars.isEmpty) nrOpen == 0 // Base case reached, verify that nr '(' equal ')'
      else if (chars.head == '(') compareParenthesis(nrOpen + 1, chars.tail) // New '('
      else if (chars.head == ')')
        if (nrOpen > 0) compareParenthesis(nrOpen - 1, chars.tail) // New ')'
        else false // No previous '(' so incorrect syntax
      else compareParenthesis(nrOpen, chars.tail) // For any non-parentheses char just continue

    compareParenthesis(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    /**
     * Sequential.
     * For maximum performance, use a while loop in the traverse method,
     * or make traverse tail-recursive -- do not use a Range.
     */
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until) (arg1, arg2)
      else chars(idx) match {
        case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
        case ')' =>
          if (arg1 > 0) traverse(idx + 1, until, arg1 - 1, arg2)
          else traverse(idx + 1, until, arg1, arg2 + 1)
        case _ => traverse(idx + 1, until, arg1, arg2)
      }
    }

    /**
     * Parallel.
     * Sections with size smaller or equal to the threshold should be
     * processed sequentially.
     */
    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (a1, a2) = parallel(reduce(from, mid), reduce(mid, until))
        (a1._1 + a2._1, a1._2 + a2._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }
}
