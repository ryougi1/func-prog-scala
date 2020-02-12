package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {

    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println("Balance:")
    println(balance("(if (zero? x) max (/ 1 x))".toList)) //true
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)) //true
    println(balance("this is just a (quick) example".toList)) //true
    println(balance("()(())".toList)) //true

    println(balance(":-)".toList)) //false
    println(balance("())(".toList)) //false
    println(balance("this is (in))correct!".toList)) // false
    println(balance("())(".toList)) //false
    println(balance(")(".toList)) //false

    println("Counting Change:")
    println("Way to give change for 4, [1,2] = " + countChange(4, List[Int](1, 2)))
    println("Way to give change for 6, [1,2, 3] = " + countChange(6, List[Int](1, 2, 3)))
  }

  /**
   * Exercise 1
   */
  //     1
  //    1 1
  //   1 2 1
  //  1 3 3 1
  // 1 4 6 4 1
  //1 5 10 10 5 1
  def pascal(c: Int, r: Int): Int =
  // Value is the sum of pascal(c-1, r-1) + pascal(c, r-1)
  // If c = 0 or c = row then value = 1
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    // Using isEmpty, head, tail
    // Conditions for balance are:
    // Number of '(' are the same as ')'.
    // Anytime a ')' occurs, there must have been a preceding '(' that's not already closed.

    def compareParenthesis(nrOpen: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty) nrOpen == 0 // Base case reached, verify that nr '(' equal ')'
      else if (chars.head == '(') compareParenthesis(nrOpen + 1, chars.tail) // New '('
      else if (chars.head == ')')
        if (nrOpen > 0) compareParenthesis(nrOpen - 1, chars.tail) // New ')'
        else false // No previous '(' so incorrect syntax
      else compareParenthesis(nrOpen, chars.tail) // For any non-parentheses char just continue

    compareParenthesis(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
  // Have unlimited supply of the coin denominations given.
  // Hint: How many ways can you give change for 0? How many ways can you give change for >0 if you have no coins?
    if (money == 0) 1 // Base case success
    // Condition money < 1 could be replaced with money < coins.head to save a few recursions
    // Only works if list is sorted
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)

}
