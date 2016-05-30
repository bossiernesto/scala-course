package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (c, r) if c == 0 || r == c || r == 0 => 1
    case _                                    => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def iterate(chars: List[Char], acum: Int): Int = {
      (chars, acum) match {
        case (chars, acum) if (chars.isEmpty || acum < 0) => acum
        case (x::xs, acum) => x match {
          case '(' => iterate(xs, acum + 1)
          case ')' => iterate(xs, acum - 1)
          case _  => iterate(xs, acum)
        }
      }
    }
    
    iterate(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (0, _) => 1
      case (m, c) if m < 0 || c.isEmpty => 0
      case (m, c) if m <= 0 && !c.isEmpty => 0
      case (m, c) => countChange(m, c.tail) + countChange(m - c.head, c)
      
    }
    
  }
}
