package no.penger.crud

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/**
 * A Cell is the mapping of a type to/from the web.
 * link(), editable() and fixed() provides three different ways to render,
 * while tryCast() parses a string back to the given type so it can be persisted.
 */
trait cells extends viewFormat {

  trait Cell[E] {
    def link(ctx: String, e:E): ElemFormat
    def editable(e: E):         ElemFormat
    def fixed(e: E):            ElemFormat
    def tryCast(value: String): Try[E]
  }

  /* homogeneous error handling for simple types */
  abstract class ValueCell[E: ClassTag] extends Cell[E] {
    final def tryCast(value:String): Try[E] =
      Try(cast(value)) match {
        case Failure(f) => Failure(new RuntimeException(s"$value is not a valid ${implicitly[ClassTag[E]].runtimeClass}", f))
        case success    => success
      }
    protected def cast(value: String): E
  }

  /**
   * A collection of 'Cell's, one for each column of a database row.
   *  This means that in order create an instance of this, you need 'Cell's for every
   *  type in the row
   * @tparam PROJECTION
   */
  @annotation.implicitNotFound("Couldn't find cell instances for all the types in projection ${PROJECTION}")
  trait CellRow[PROJECTION]{
    def cells:List[Cell[_]]

    /* given an instance of 'PROJECTION', pick out the fields that correspond to each cell in 'cells' */
    def unpackValues(e:PROJECTION):List[Any]

    /* reconstruct a PROJECTION given a list of values */
    def packValues(a: Seq[Any]): PROJECTION
  }

  implicit def singleRow[T](implicit c: Cell[T]): CellRow[T] = new CellRow[T]{
    override def cells = List(c)
    override def packValues(vs: Seq[Any]): T = vs.head.asInstanceOf[T]
    override def unpackValues(e: T): List[Any] = List(e)
  }
}