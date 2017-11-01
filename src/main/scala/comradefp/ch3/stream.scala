package ch3.stream

sealed trait Stream[+A]

object Stream {

}

private[stream] final class StreamOps[A](stream: Stream[A]) {
  
}
