package chapter4








trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(r) =>  Right(f(r))
      case Left(a) => Left(a)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(_) => b
        case Right(a) => Right(a)
      }
  }


  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):  Either[EE, C] = {

     for {
       a <- this;
       b1 <- b
     } yield f(a,b1)
  }

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }
}


case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]