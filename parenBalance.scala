// not investigated a better way!

object TestParenBal extends App {
  def balance(chars: List[Char]): Boolean = {
    def go(c: List[Char], acc: Int): Int  = c match {
      case h :: t => if(h == '(') go(t,acc+1) else if(h ==')') go(t,acc-1) else go(t,acc)
      case _ => acc 
    }
   if( go(chars,0) == 0)
     true
   else
     false
  }

  if (args.length == 0)
    println("enter some string")
  else
    println("result is " + balance(args(0).toList))
}
