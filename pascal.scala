  //not this is not efficient way
object TestPascal extends App{

  def pascal(c: Int, r: Int): Int = {
    //@annotation.tailrec 
    def go (col: Int, row: Int) : Int = (col,row) match {
      case (0,_) => 1 
      case (a,b) => if (a == b) 1 else go(a-1,b-1) + go(a,b-1)
    }
    go(c,r)

  }
  if (args.length <= 1 || args(0).toInt > args(1).toInt)
    println("""enter column int folled by row int, e.g 4 5 ,
    also col cannot be greater than row""")
  else
    println(pascal(args(0).toInt,args(1).toInt))
  //this is from stack overflow (thanks). this code feeds back
  //the previous sequence for n iterations e.g
  //1 => (0 ,1) + (1 ,0) = (1,1)
  //(1,1) => (0,1,1) + (1,1,0) = (1,2,1)
  //(1,2,1) => (0,1,2,1) + (1,2,1,0) = 1,3,3,1)
  val p = Stream.iterate(Seq(1))(a=>(0+:a,a:+0).zipped.map(_+_))
  val row =args(1).toInt
  val col = args(0).toInt
  val ls = p(row)
  println(ls)
  println(ls(col))
}
