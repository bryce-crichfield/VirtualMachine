trait Result[+A]
case class Success[A](value: A) extends Result[A]
case class Failure(message: String) extends Result[Nothing]

trait Attempt[+A] {
    def apply(): Result[A]

    def map[B](f: A => B): Attempt[B] = { () =>
        this() match {
            case Success(value)   => Success(f(value))
            case Failure(message) => Failure(message)
        }
    }

    def flatMap[B](f: A => Attempt[B]): Attempt[B] = {
        this() match {
            case Success(value)   => f(value)
            case Failure(message) => () => Failure(message)
        }
    }
}

object Attempt {
    def success[A](value: A): Attempt[A] = { () => Success(value) }

    def failure[A](message: String): Attempt[A] = { () => Failure(message) }

    def tap(f: => Unit): Attempt[Unit] = { () =>
        {
            f
            Success(())
        }
    }
}
