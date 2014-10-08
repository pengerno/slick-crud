package no.penger

import scala.util.{Failure, Success, Try}

package object crud {

  trait CrudAbstract extends editors with cellRowInstances

  trait Crud
    extends CrudAbstract
    with html.cellInstances
    with html.viewHtml
    with http.unfilteredIntegration

  case class Update(column: ColumnName, oldValue: Any, newValue: Any, numUpdated: Int)

  case class FailedUpdate(column: ColumnName, value: String, t: Throwable)

  case class ColumnName(asString: String) extends AnyVal{
    override def toString: String = asString
  }

  case class TableName(asString: String) extends AnyVal {
    override def toString: String = asString
  }

  def sequence[L, R](result: Iterable[Either[L, R]]): Either[Seq[L], Seq[R]] =
    result.foldLeft[Either[Seq[L], Seq[R]]](Right(Seq.empty)){
      case (Right(acc), Right(u)) ⇒ Right(acc :+ u)
      case (Left(acc),  Left(f))  ⇒ Left(acc :+ f)
      case (Left(acc),  _)        ⇒ Left(acc)
      case (_,          Left(f))  ⇒ Left(Seq(f))
    }

  implicit class EitherToTry[T](val e: Try[T]) extends AnyVal {
    def toEither = e match {
      case Success(t) ⇒ Right(t)
      case Failure(f) ⇒ Left(f)
    }
  }

  implicit class OptionToTry[T](val ot: Option[T]) extends AnyVal {
    def toTry(leftMsg: String) = ot map Success.apply getOrElse Failure(new RuntimeException(leftMsg))
  }
}
