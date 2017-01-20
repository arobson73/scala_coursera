
//note i misread this question to mean how many ways 
//can money be changed using the available coins.
//
//its should be interpreted as how many ways can money be changed
//using the denominations in coins

object TestCountChange extends App {
  def countChangePos(money: Int, coins: List[Int]): Int = {
    val p = powerset(coins)
//    println(s"power set is $p")
    val s = p.map(a => a.foldRight(0)(_+_))
    val r = s.map(a => a == money).filter(_ == true).size
    r
  }
  def powerset(lst: List[Int]): List[List[Int]] = {
    def pwr(lst: List[Int], acc: List[List[Int]]): List[List[Int]] = lst match {
      case Nil => acc
      case x :: xs => pwr(xs, acc ::: (acc map (x :: _)))
    }
    pwr(lst,Nil::Nil)
  }
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(m: Int, c: List[Int]) : Int = {
      if (c.isEmpty) 0
      else if (m - c.head == 0) 1
      else if (m - c.head < 0) 0
      else countChange(m - c.head, c) + countChange(m, c.tail)
    }
    count(money, coins.sorted.distinct)
  //  count(money, coins)
  }


  if (args.length == 0)
    println("Enter total amount (interger), and then series of coins")
  else
  {
    val m = args.head.toInt
    println(s"head is $m")
    val c = args.tail.toList.map(a => a.toInt)
    println(s"coins are $c")
    val res = countChangePos(m,c)
    println(s"Can it be changed, how many ways is $res")
    val res2 = countChange(m,c)
    println(s"How many denominations, $res2")
  }
}
