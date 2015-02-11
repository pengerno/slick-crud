package no.penger

package object crud{

  type AbstractTable[T] = scala.slick.lifted.AbstractTable[T]

  /* work around limitations in scala stdlib, without taking scalaz dependency */

  private [crud] def sequence[L, R](result: Iterable[Either[L, R]]): Either[Seq[L], Seq[R]] =
    result.foldLeft[Either[Seq[L], Seq[R]]](Right(Seq.empty)){
      case (Right(acc), Right(u)) ⇒ Right(acc :+ u)
      case (Left(acc),  Left(f))  ⇒ Left(acc :+ f)
      case (Left(acc),  _)        ⇒ Left(acc)
      case (_,          Left(f))  ⇒ Left(Seq(f))
    }

  private[crud] implicit class EitherX[L, R](val e: Either[L, R]) extends AnyVal {
    /* make 'Either' right biased*/
    def foreach(f: R => Unit): Unit = {e.right.foreach(f); ()}
    def map[RR](f: R => RR): Either[L, RR] = e.right.map(f)
    def flatMap[RR](f: R => Either[L, RR]) = e.right.flatMap(f)
    
    def biMap[LL, RR](left: L ⇒ LL, right: R ⇒ RR): Either[LL, RR] = e match {
      case Right(r) ⇒ Right(right(r))
      case Left(l)  ⇒ Left(left(l))
    }
  }

  private[crud] implicit class ListX[T](val ts: List[T]) extends AnyVal {
    def zipMap[U](f: T ⇒ U): List[(U, T)] = ts.map(t ⇒ (f(t), t))
  }

  private[crud] implicit class AnyX[A](val a: A) extends AnyVal {
    /* typesafe equals */
    def =:=(aa: A): Boolean = a == aa
    def =/=(aa: A): Boolean = a != aa

    /* apply side effect and return value */
    def andThen(f: A ⇒ Unit) = {
      f(a)
      a
    }
  }

  implicit def any2stringadd(x: Option[Unit]): Option[Unit] = x
}
