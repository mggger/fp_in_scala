package chapter4



import scala.{Option => _, Either => _, _}


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(
      a1 => b.map(
        b1 => f(a1, b1)
      )
    )
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

}