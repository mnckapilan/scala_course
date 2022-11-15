package recfun

import scala.annotation.tailrec
import scala.language.postfixOps

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1ß
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else if (c < 0 || r < 0) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanced(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (open < 0) false
      else if (chars.head == '(') balanced(chars.tail, open + 1)
      else if (chars.head == ')') balanced(chars.tail, open - 1)
      else balanced(chars.tail, open)
    }

    balanced(chars, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else count(money, coins.tail) + count(money - coins.head, coins)
    }

    count(money, coins)
  }
}
