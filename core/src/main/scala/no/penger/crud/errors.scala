package no.penger.crud

import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions

trait errors {
  sealed trait Error
  case class ErrorMsg(msg: String) extends Error
  case class ErrorExc(t: Throwable) extends Error

  /* grr type inference */
  def errorMsg(msg: String): Error = new ErrorMsg(msg)
  def errorExc(t: Throwable): Error = new ErrorExc(t)

  private[crud] implicit class OptionX[T](val ot: Option[T]) {
    def orError(msg: String) = ot map (Right(_)) getOrElse Left(errorMsg(msg))
  }

  private[crud] implicit class TryX[T](val e: Try[T]) {
    def toEither[L](left: Throwable ⇒ L): Either[L, T] = e match {
      case Success(t) ⇒ Right(t)
      case Failure(f) ⇒ Left(left(f))
    }
  }
}
