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
    def link(ctx: String, e:E): ViewFormat
    def editable(e: E):         ViewFormat
    def fixed(e: E):            ViewFormat
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
}