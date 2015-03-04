package no.penger.crud

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait cells extends booleanLike with errors {

  class CacheLookup{
    val sqlCache = collection.mutable.Map.empty[String, Option[Seq[Any]]]
  }

  /**
   * A Cell is the mapping of a type to/from the stringly typed web.
   */
  sealed trait Cell[E] {
    val isOptional:  Boolean
    val isEditable:  Boolean
    val typeName:    String
    def toStr(e: E): String
    def fromStr(value: String): Either[Error, E]
    def constrainedValues: Option[CacheLookup ⇒ Option[Seq[E]]]

  }

  /* homogeneous error handling and type name via ClassTag */
  case class SimpleCell[E: ClassTag](asString:   E      ⇒ String,
                                     fromString: String ⇒ E,
                                     inputType:  String          = "text",
                                     isEditable: Boolean         = true) extends Cell[E] {

    override val isOptional             = false
    override val typeName               = implicitly[ClassTag[E]].runtimeClass.getSimpleName
    override def toStr(e: E)            = asString(e)
    override def constrainedValues      = None
    override def fromStr(value: String) = Try(parse(value)) match {
      case Success(v) ⇒ Right(v)
      case Failure(_) ⇒ Left(errorMsg(s"'$value' is not a valid ${implicitly[ClassTag[E]].runtimeClass}"))
    }
    def parse(s: String): E  = fromString(s)
  }

  case class BooleanCell[E: BooleanLike: ClassTag](asString: E ⇒ String, fromString: String ⇒ E)(possibleValues: Seq[E]) extends Cell[E] {
    def isTrue(a: E) = implicitly[BooleanLike[E]].isTrue(a)

    override val isEditable             = true
    override val isOptional             = false
    override val typeName               = implicitly[ClassTag[E]].runtimeClass.getSimpleName
    override def toStr(b: E)            = asString(b)
    override def fromStr(value: String) = Try(fromString(value)).toEither(t ⇒ errorMsg(s"$value is not a valid boolean"))
    override def constrainedValues      = Some(_ ⇒ Some(possibleValues))
  }

  case class ConstrainedCell[E <: Any](wrapped: Cell[E], selectStatement: Option[String] = None)(_possibleValues: ⇒ Option[Seq[E]]) extends Cell[E]{
    override val isEditable             = wrapped.isEditable
    override val isOptional             = wrapped.isOptional
    override val typeName               = s"Enum[${wrapped.typeName}]"
    override def toStr(e: E)            = wrapped.toStr(e)
    override def fromStr(value: String) = wrapped.fromStr(value)
    override def constrainedValues      = Some(
      (c: CacheLookup) ⇒ c.sqlCache.getOrElseUpdate(selectStatement.fold(typeName)(typeName + _), _possibleValues).asInstanceOf[Option[Seq[E]]]
    )
  }

  case class PKCell[A](wrapped: Cell[A]) extends Cell[A] {
    override val typeName               = s"PK[${wrapped.typeName}]"
    override val isEditable             = wrapped.isEditable
    override val isOptional             = wrapped.isOptional
    override val constrainedValues      = None
    override def fromStr(value: String) = wrapped.fromStr(value)
    override def toStr(e: A)            = wrapped.toStr(e)
  }

  case class OptionCell[A](wrapped: Cell[A]) extends Cell[Option[A]] {
    override val isEditable             = wrapped.isEditable
    override val isOptional             = true
    override val typeName               = s"Option[${wrapped.typeName}]"
    override def toStr(e: Option[A])    = e map wrapped.toStr getOrElse ""
    override def constrainedValues      =
      wrapped.constrainedValues.map(_.andThen(_.map(_.map(Option(_)))))

    override def fromStr(value: String) = Option(value.trim).filterNot(_.isEmpty) match {
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