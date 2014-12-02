package no.penger.crud

import scala.reflect.ClassTag
import scala.slick.lifted.{Column, Query}
import scala.util.{Failure, Success, Try}

trait cells extends errors {

  /**
   * A Cell is the mapping of a type to/from the stringly typed web.
   */
  trait Cell[E] {
    val inputType:   String
    val isEditable:  Boolean
    val alignRight:  Boolean
    val typeName:    String
    def toStr(e: E): String
    def fromStr(value: String): Either[Error, E]
  }

  object Cell {
    def toStr[T](t: T)(implicit c: Cell[T]) = c.toStr(t)
  }

  /* homogeneous error handling and type name via ClassTag */
  case class SimpleCell[E: ClassTag](
    asString:   E      ⇒ String,
    fromString: String ⇒ E,
    inputType:  String          = "text",
    isEditable: Boolean         = true,
    alignRight: Boolean         = true) extends Cell[E] {

    val typeName             = implicitly[ClassTag[E]].runtimeClass.getSimpleName
    def toStr(e: E)          = asString(e)
    def parse(s: String): E  = fromString(s)

    final def fromStr(value: String): Either[Error, E] =
      Try(parse(value)) match {
        case Success(v) ⇒ Right(v)
        case Failure(_) ⇒ Left(errorMsg(s"'$value' is not a valid ${implicitly[ClassTag[E]].runtimeClass}"))
      }
  }

  case class PKCell[A](wrapped: Cell[A]) extends Cell[A] {
    override val isEditable             = wrapped.isEditable
    override val alignRight             = wrapped.alignRight
    override val inputType              = wrapped.inputType
    override val typeName               = s"PK[${wrapped.typeName}]"
    override def toStr(e: Id[A])        = wrapped.toStr(e)
    override def fromStr(value: String) = wrapped.fromStr(value)
  }

  class FkCache{
    val sqlCache = collection.mutable.Map.empty[String, Seq[Any]]
  }

  case class FKCell[A](wrapped: Cell[A], selectStatement: String)(_possibleValues: ⇒ Seq[A]) extends Cell[A] {
    override val isEditable             = wrapped.isEditable
    override val alignRight             = wrapped.alignRight
    override val inputType              = wrapped.inputType
    override val typeName               = s"FK[${wrapped.typeName}]"
    override def toStr(e: Id[A])        = wrapped.toStr(e)
    override def fromStr(value: String) = wrapped.fromStr(value)
    def possibleValues(c: FkCache): Seq[A] = c.sqlCache.getOrElseUpdate(selectStatement, _possibleValues).asInstanceOf[Seq[A]]
  }
  object FKCell{
    def apply[A](selectStatement: String, possibleValues: ⇒ Seq[A])(wrapped: Cell[A]): FKCell[A] =
      new FKCell(wrapped, selectStatement)(possibleValues)
  }

  case class OptionCell[A](wrapped: Cell[A]) extends Cell[Option[A]] {
    override val isEditable           = wrapped.isEditable
    override val alignRight           = wrapped.alignRight
    override val inputType            = wrapped.inputType
    override val typeName             = s"Option[${wrapped.typeName}]"
    override def toStr(e: Option[A]) = e map wrapped.toStr getOrElse ""

    final def fromStr(value: String): Either[Error, Option[A]] = Option(value.trim).filterNot(_.isEmpty) match {
      case Some(v) ⇒ wrapped.fromStr(value).right.map(Some(_))
      case None    ⇒ Right(None)
    }
  }

  implicit val booleanCell = SimpleCell[Boolean](_.toString, _.toBoolean, inputType = "checkbox")
  implicit val doubleCell  = SimpleCell[Double] (_.toString, _.toDouble,  inputType = "number")
  implicit val intCell     = SimpleCell[Int]    (_.toString, _.toInt,     inputType = "number")
  implicit val longCell    = SimpleCell[Long]   (_.toString, _.toLong,    inputType = "number")
  implicit val stringCell  = SimpleCell[String] (identity,   _.ensuring(_.nonEmpty))

  /**
   * A collection of 'Cell's, one for each column of a database row.
   *  This means that in order create an instance of this, you need 'Cell's for every
   *  type in the row
   */
  @annotation.implicitNotFound("Couldn't find cell instances for all the types in projection ${P}")
  trait CellRow[P]{
    def cells:List[Cell[_]]

    /* given an instance of 'P', pick out the fields that correspond to each cell in 'cells' */
    def unpackValues(e:P):List[Any]

    /* reconstruct a P given a list of values */
    def packValues(a: Seq[Any]): P
  }
}