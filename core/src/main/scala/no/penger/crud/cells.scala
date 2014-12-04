package no.penger.crud

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait cells extends booleanLike with errors {

  class CacheLookup{
    val sqlCache = collection.mutable.Map.empty[String, Seq[Any]]
  }

  /**
   * A Cell is the mapping of a type to/from the stringly typed web.
   */
  sealed trait Cell[E] {
    val inputType:   String
    val isEditable:  Boolean
    val alignRight:  Boolean
    val typeName:    String
    def toStr(e: E): String
    def fromStr(value: String): Either[Error, E]
    def constrained: Option[CacheLookup ⇒ Seq[E]]
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

    override val constrained            = None
    override val typeName               = implicitly[ClassTag[E]].runtimeClass.getSimpleName
    override def toStr(e: E)            = asString(e)
    override def fromStr(value: String) =
      Try(parse(value)) match {
        case Success(v) ⇒ Right(v)
        case Failure(_) ⇒ Left(errorMsg(s"'$value' is not a valid ${implicitly[ClassTag[E]].runtimeClass}"))
      }
    def parse(s: String): E  = fromString(s)
  }

  trait WrappedCell[E] extends Cell[E]{
    def wrapped: Cell[E]
    override val alignRight             = wrapped.alignRight
    override val constrained            = wrapped.constrained
    override val inputType              = wrapped.inputType
    override val isEditable             = wrapped.isEditable
    override def toStr(e: E)            = wrapped.toStr(e)
    override def fromStr(value: String) = wrapped.fromStr(value)
  }

  trait ConstrainedCell[E] extends WrappedCell[E]{
    override final val inputType = "select"
  }

  case class PKCell[A](wrapped: Cell[A]) extends WrappedCell[A] {
    override val typeName = s"PK[${wrapped.typeName}]"
  }

  case class FKCell[A](wrapped: Cell[A], selectStatement: String)(_possibleValues: ⇒ Seq[A]) extends ConstrainedCell[A] {
    override val typeName = s"FK[${wrapped.typeName}]"

    override val constrained = Some(possibleValues _)

    def possibleValues(c: CacheLookup) =
      c.sqlCache.getOrElseUpdate(selectStatement, _possibleValues).asInstanceOf[Seq[A]]
  }

  object FKCell{
    def apply[A](selectStatement: String, possibleValues: ⇒ Seq[A])(wrapped: Cell[A]): FKCell[A] =
      new FKCell(wrapped, selectStatement)(possibleValues)
  }

  case class EnumCell[A](wrapped: Cell[A], possibleValues: Seq[A]) extends ConstrainedCell[A] {
    override val typeName    = s"Enum[${wrapped.typeName}]"
    override val constrained = Some((_: CacheLookup) ⇒ possibleValues)
  }

  case class SelectCell[A: BooleanLike: ClassTag](asString: A ⇒ String, fromString: String ⇒ A) extends Cell[A] {
    override val alignRight             = true
    override val constrained            = None
    override val isEditable             = true
    override val inputType              = "select"
    override val typeName               = implicitly[ClassTag[A]].runtimeClass.getSimpleName
    override def toStr(b: A)            = asString(b)
    override def fromStr(value: String) =
      Try(fromString(value)).toEither(t ⇒ errorMsg(s"$value is not a valid boolean"))
  }

  case class OptionCell[A](wrapped: Cell[A]) extends Cell[Option[A]] {
    override val constrained         = wrapped.constrained.map(_.andThen(as ⇒ (as map Option.apply) :+ None))
    override val isEditable          = wrapped.isEditable
    override val alignRight          = wrapped.alignRight
    override val inputType           = wrapped.inputType
    override val typeName            = s"Option[${wrapped.typeName}]"
    override def toStr(e: Option[A]) = e map wrapped.toStr getOrElse ""

    final def fromStr(value: String): Either[Error, Option[A]] = Option(value.trim).filterNot(_.isEmpty) match {
      case Some(v) ⇒ wrapped.fromStr(value).right.map(Some(_))
      case None    ⇒ Right(None)
    }
  }

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