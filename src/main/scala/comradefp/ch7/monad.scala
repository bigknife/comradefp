package monad

sealed trait Free[F[_], A] {self ⇒
  def flatMap[B](f: A ⇒ Free[F, B]): Free[F, B] =
    FlatMap(self, (a: A) ⇒ f(a))

  def pure[T](a: T): Free[F, T] = Return(a)

  def map[B](f: A ⇒ B): Free[F, B] =
    flatMap(a ⇒ pure(f(a)))
}

case class Return[F[_], A](given: A) extends Free[F, A]
case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](free: Free[F, A], f: A ⇒ Free[F, B]) extends Free[F, B]


sealed trait Todo[A]
case class NewTask[A](task: A) extends Todo[A]
case class CompleteTask[A](task: A) extends Todo[A]
case class GetTasks[A](default: A) extends Todo[A]

object Todo {
  def newTask[A](task: A): Free[Todo, A] = Suspend(NewTask(task))
  def completeTask[A](task: A): Free[Todo, A] = Suspend(CompleteTask(task))
  def getTasks[A](default: A): Free[Todo, A] = Suspend(GetTasks(default))
}

object TodoApp extends App {
  import Todo._
  lazy val todos: Free[Todo, Map[String, Boolean]] =
    for {
      _ ← newTask("Go to scala days")
      _ ← newTask("Write a novel")
      _ ← newTask("Meet Tina Fey")
      _ ← completeTask("Go to scala days")
      tasks ← getTasks(default = Map.empty[String, Boolean])
    } yield tasks

  println(todos)
}
