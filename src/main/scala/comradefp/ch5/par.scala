package object par {
  type ExecutorService = java.util.concurrent.ExecutorService
  type Future[A]       = java.util.concurrent.Future[A]

  type Par[A] = ExecutorService => Future[A]

  // companion object
  object Par { outter =>

    private[par] case class UnitFuture[A](get: A) extends Future[A] {
      def isDone: Boolean                                            = true
      def cancel(mayInterruptIfRunning: Boolean): Boolean            = false
      def get(timeout: Long, unit: java.util.concurrent.TimeUnit): A = get
      def isCancelled: Boolean                                       = false
    }

    def unit[A](a: A): Par[A] =
      (es: ExecutorService) => UnitFuture(a)

    def run[A](a: Par[A])(es: ExecutorService): Future[A] = a(es)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val pa = a(es)
        val pb = b(es)
        UnitFuture(f(pa.get, pb.get))
      }

    def map[A, B](a: Par[A])(f: A => B): Par[B] =
      map2(a, unit(()))((a, _) => f(a))

    def fork[A](a: => Par[A]): Par[A] =
      (es: ExecutorService) => {
        es.submit(new java.util.concurrent.Callable[A] {
          def call: A = a(es).get
        })
      }

    def join[A](aa: Par[Par[A]]): Par[A] = (es: ExecutorService) => {
      val a: Future[Par[A]] = aa(es)
      val b: Par[A]         = a.get()
      b(es)
    }

    def asyncF[A, B](f: A => B): A => Par[B] =
      (a: A) => lazyUnit(f(a))

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      as.foldRight(unit(List[A]()))((a, acc) => {
        map2(a, acc)(_ :: _)
      })


    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    implicit final class ParOps[A](a: Par[A]) {
      def run(es: ExecutorService): Future[A]           = outter.run(a)(es)
      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = outter.map2(a, b)(f)

      def fork: Par[A] = outter.fork(a)

      def map[B](f: A => B): Par[B]  = outter.map(a)(f)
      def flatMap[B](f: A => Par[B]) = outter.flatMap(a)(f)
    }
  }
}

package par {
  import Par._
  import java.util.concurrent._

  object Demo {
    // 在当前线程执行分治计算
    def sum_0(ints: IndexedSeq[Int]): Int =
      if (ints.size <= 1) ints.headOption.getOrElse(0)
      else {
        val (l, r) = ints.splitAt(ints.size / 2)
        sum_0(l) + sum_0(r)
      }

    // 在另外的线程进行并行计算 ver1
    /*
    def sum_1(ints: IndexedSeq[Int]): Int =
      if (ints.size <= 1) ints.headOption.getOrElse(0)
      else {
        val (l, r) = ints.splitAt(ints.size / 2)
        val sumL   = Par.unit(sum_1(l)) // 并行化?
        val sumR   = Par.unit(sum_1(r)) // 并行化?
        Par.get(sumL) + Par.get(sumR) // 并行化?
      }
     */

    // 在另外的线程进行并行计算 ver2
    /*
    def sum_2(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.size <= 1) Par.unit(ints.headOption.getOrElse(0))
      else {
        val (l, r) = ints.splitAt(ints.size / 2)
        Par.map2(sum_2(l), sum_2(r))(_ + _)
      }
     */

    // 在另外的线程进行并行计算 ver3 fork
    def sum_3(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.size <= 1) Par.unit(ints.headOption.getOrElse(0))
      else {
        val (l, r) = ints.splitAt(ints.size / 2)

        Par.map2(sum_3(l).fork, sum_3(r).fork) {
          case (x, y) =>
            x + y
        }
      }

    def getUrl(url: String): String = {
      import java.net._, java.io._
      val resource = new URL(url)
      val conn     = resource.openConnection()

      val content = new StringBuffer()
      val reader  = new BufferedReader(new InputStreamReader(conn.getInputStream()))

      def go(reader: BufferedReader, content: StringBuffer): StringBuffer = {
        val line = reader.readLine()
        if (line == null) content
        else {
          content.append(line)
          go(reader, content)
        }
      }

      go(reader, content)
      reader.close
      content.toString()

    }

    def readSomeUrls(): Par[Map[String, String]] = {
      val asyncGetUrl: String => Par[String] = Par.asyncF(getUrl)
      for {
        c1 <- asyncGetUrl("http://zrzking.iteye.com/blog/1943722?" + scala.util.Random.nextInt())
        c2 <- asyncGetUrl("http://zrzking.iteye.com/blog/1943722?" + scala.util.Random.nextInt())
        c3 <- asyncGetUrl("http://zrzking.iteye.com/blog/1943722?" + scala.util.Random.nextInt())
        c4 <- asyncGetUrl("http://zrzking.iteye.com/blog/1943722?" + scala.util.Random.nextInt())
      } yield Map("baidu" -> c1, "v2ex" -> c2)
    }

    var flag: Long       = System.currentTimeMillis()
    def tap: Unit        = flag = System.currentTimeMillis()
    def timePassed: Long = System.currentTimeMillis() - flag

    def testSync(): Unit = {
      tap
      val m = Map(
        "baidu"       -> getUrl("http://zrzking.iteye.com/blog/1943722?" + scala.util.Random.nextInt()),
        "v2ex"        -> getUrl("http://zrzking.iteye.com/blog/1943722?" + scala.util.Random.nextInt()),
        "v2ex_412436" -> getUrl("http://zrzking.iteye.com/blog/1943722?" + scala.util.Random.nextInt()),
        "v2ex_a"      -> getUrl("http://zrzking.iteye.com/blog/1943722?" + scala.util.Random.nextInt())
      )
      m.size
      println(timePassed)
      ()
    }
    /**
val urls = List("http://news.163.com/17/1206/11/D4VGUKP2000189FH.html", "http://news.163.com/17/1206/10/D4VDKFVS0001899O.html", "http://news.163.com/17/1206/14/D4VT7PV7000187V9.html", "http://news.163.com/17/1206/14/D4VSIONN0001875P.html").map(asyncF(getUrl))
      */

    def testAsync(): Unit = {
      tap
      val es = Executors.newFixedThreadPool(20)
      val p  = readSomeUrls().run(es).get
      p.size
      println(timePassed)
      es.shutdownNow()
      ()
    }

  }
}
