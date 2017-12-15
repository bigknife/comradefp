package laziness

object demo {
  def sum(a1: Int, a2: Int): Int = {
    println("start execute sum")
    a1 + a2
  }

  def sum1(a1: ⇒ Int, a2: ⇒ Int): Int = {
    println("start execute sum1")
    a1 + a2
  }

  def test(): Unit = {
    val s1 = sum1({throw new Exception("a1")}, {throw new Exception("a2")})
    println("s1 declared")
    val s2 = sum1({throw new Exception("a1")}, {throw new Exception("a2")})
    println("s1 declared")
    val s3 = s1 + s2
    println(s"s3 is $s3")
  }
}
