package ch1

/**
  * Usage:
  * {
  *    import ch1.fib
  *    fib(n)
  * }
  *
  * 使用对象的`apply`方法模拟函数调用
  */
object fib {
  def apply(n: Long): Long = {
    def go(n: Long, a: Long, b: Long): Long = n match {
      case 0 => 0
      case 1 => b
      case x => go(n - 1, b, a + b)
    }

    go(n, 0, 1)
  }
}
