package no.penger

import scala.util.{Failure, Success, Try}

package object crud {

  trait CrudAbstract extends editors with cellRowInstances

  case class ColumnName(override val toString: String) extends AnyVal
  case class TableName(override val toString: String) extends AnyVal

  sealed trait Error
  case class ErrorMsg(msg: String) extends Error
  case class ErrorExc(t: Throwable) extends Error

  private[crud] implicit class OptionX[T](val ot: Option[T]) extends AnyVal {
    def orError(msg: String): Either[Error, T] = ot map (Right(_)) getOrElse Left(ErrorMsg(msg))
  }

  /* the rest work around limitations in scala stdlib, without taking scalaz dependency */

  private [crud] def sequence[L, R](result: Iterable[Either[L, R]]): Either[Seq[L], Seq[R]] =
    result.foldLeft[Either[Seq[L], Seq[R]]](Right(Seq.empty)){
      case (Right(acc), Right(u)) ⇒ Right(acc :+ u)
      case (Left(acc),  Left(f))  ⇒ Left(acc :+ f)
      case (Left(acc),  _)        ⇒ Left(acc)
      case (_,          Left(f))  ⇒ Left(Seq(f))
    }

  private[crud] implicit class TryX[T](val e: Try[T]) extends AnyVal {
    def toEither = e match {
      case Success(t) ⇒ Right(t)
      case Failure(f) ⇒ Left(f)
    }
  }

  private[crud] implicit class EitherX[L, R](val e: Either[L, R]) extends AnyVal {
    /* make 'Either' right biased*/
    def foreach[U](f: R => U): Unit = e.right.foreach(f)
    def map[RR](f: R => RR): Either[L, RR] = e.right.map(f)
    def flatMap[RR](f: R => Either[L, RR]) = e.right.flatMap(f)

    def sideEffects(left: L ⇒ Unit, right: R ⇒ Unit) = {
      e.fold(left, right)
      e
    }

    def mapBoth[LL, RR](left: L ⇒ LL, right: R ⇒ RR): Either[LL, RR] = e match {
      case Right(r) ⇒ Right(right(r))
      case Left(l)  ⇒ Left(left(l))
    }
  }

  /* typesafe equals */
  private[crud] implicit class Equals[A](val a: A) extends AnyVal {
    def =:=(b: A): Boolean = a == b
    def =/=(b: A): Boolean = a != b
  }
}
