object TestFuncSets extends App {
  type Set = Int => Boolean
  //make lazy since global is not working!
  lazy val bound  = 1000
  //val ltz = TestFuncSets.LessThanZeroSet
  //val r = TestFuncSets.contains(ltz,4)
  //val r2 = TestFuncSets.contains(ltz,-3)
  def contains(s: Set, elem: Int): Boolean =  s(elem)
  //TestFuncSets.SingletonSet(4)(3)
  //TestFuncSets.SingletonSet(4)(4)

  def SingletonSet(in:Int): Set = i => i == in
  //val ss1 = TestFuncSets.SingletonSet(3)
  //val ss2 = TestFuncSets.SingletonSet(4)
  //val uf = TestFuncSets.union(ss1,ss2)(6)
  //val ut = TestFuncSets.union(ss1,ss2)(3)
  //
  def LessThanZeroSet: Set = i => i < 0
  def GreaterThanZeroSet: Set = i => i > 0

  def evenAndLessThan10Set: Set = i => ((i % 2) ==0) && (i < 10)

  def union(s1: Set,s2: Set): Set = item => s1(item) || s2(item) 
  //
  def intersect(s1: Set, s2: Set): Set = item => s1(item) && s2(item)
  //the values that are in s, but not in t
  //val a = SingletonSet(1)
  //val b = SingletoneSet(2)
  //val c = diff(a,b)(1)  only 1 gives true
  def diff(s: Set, t: Set): Set = item => s(item) && !t(item)
  //same as intersect
  //def filter(s: Set, p: Int => Boolean): Set = item => s(item) && p(item)
  //filter(LessThanZeroSet,x => x < -2)(-5)
  def filter(s: Set, p: Int => Boolean): Set = intersect(s,p) 
  //forall(evenAndLessThan10Set,a => a < 5)
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def inforall(a: Int): Boolean = {
      if (a>bound) true
      else if (diff(s,p)(a)) false
      else inforall(a+1)
    }
    inforall(-bound)
  }
  //similar to forall but stops when first true occurs
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def inexists(a: Int): Boolean = {
      if (a > bound) false
      else if (s(a) && p(a)) true
      else inexists(a+1)
    }
    inexists(-bound)
  }
  def exists1(s: Set, p: Int => Boolean): Boolean = !forall(s, (x => !p(x)))
  //map(evenAndLessThan10Set,(a) => a+1)(8)
  def map(s: Set, f: Int => Int): Set = a => exists(s,((x) => x == f(a)))  


  def toString(s: Set): String = {
    val xs = for(i <- -bound to bound if contains(s,i)) yield i
    xs.mkString("{", ",", "}")
  }

  def printSet(s: Set) =  {
    println(toString(s))
  }

}
