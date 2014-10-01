package no.penger.crud

import scala.reflect.ClassTag
import scala.util.{Success, Failure, Try}

/**
 * A Cell is the mapping of a type to/from the web.
 * link(), editable() and fixed() provides three different ways to render,
 * while tryCast() parses a string back to the given type so it can be persisted.
 */
trait cells {
  abstract class Cell[E] {
    def link(ctx: String, e:E): xml.NodeSeq
    def editable(e: E):         xml.NodeSeq
    def fixed(e: E):            xml.NodeSeq
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

  object Cell{
    /* handling of optional values*/
    implicit def optionCell[A](implicit wrapped: Cell[A]): Cell[Option[A]] = new Cell[Option[A]] {
      def link(ctx: String, e: Option[A]) =
        e.map(v => wrapped.link(ctx, v)).getOrElse(fixed(e))

      def editable(e: Option[A]) =
        e.map(wrapped.editable).getOrElse(<td contenteditable="true" align="right"></td>)

      def fixed(e: Option[A]) =
        e.map(wrapped.fixed).getOrElse(<td align="right"></td>)

      def tryCast(value: String): Try[Option[A]] = Option(value.trim).filterNot(_.isEmpty) match {
        case Some(v) => wrapped.tryCast(value).map(Some(_))
        case None    => Success(None)
      }
    }

    /* simple creation and standard rendering */
    def apply[T: ClassTag](from:      T => String,
                           to:        String => T,
                           canEdit:   Boolean      = true,
                           alignment: String       = "right") = new ValueCell[T]{
      def link(ctx: String, e: T)       = <td align={alignment}><a href={ctx + "/" + from(e)}>{from(e)}</a></td>
      def editable(e: T)                = <td contenteditable={canEdit.toString} align={alignment}>{from(e)}</td>
      def fixed(e: T)                   = <td align={alignment}>{from(e)}</td>
      protected def cast(value: String) = to(value)
    }
  }
}