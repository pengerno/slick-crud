package no.penger.crud
package html

import scala.reflect.ClassTag
import scala.util.{Success, Try}

trait cellInstances extends cells with viewFormatHtml {

  /* simple creation and standard rendering */
  def Cell[T: ClassTag](from:      T      ⇒ String,
                        to:        String ⇒ T,
                        canEdit:   Boolean      = true,
                        alignment: String       = "right") = new ValueCell[T]{
    def link(ctx: String, e: T)       = <td align={alignment}><a href={ctx + "/" + from(e)}>{from(e)}</a></td>
    def editable(e: T)                = <td contenteditable={canEdit.toString} align={alignment}>{from(e)}</td>
    def fixed(e: T)                   = <td align={alignment}>{from(e)}</td>
    protected def cast(value: String) = to(value)
  }

  /* handling of optional values*/
  implicit def optionCell[A](implicit wrapped: Cell[A]): Cell[Option[A]] = new Cell[Option[A]] {
    def link(ctx: String, e: Option[A]) =
      e.map(v ⇒ wrapped.link(ctx, v)).getOrElse(fixed(e))

    def editable(e: Option[A]) =
      e.map(wrapped.editable).getOrElse(<td contenteditable="true" align="right"></td>)

    def fixed(e: Option[A]) =
      e.map(wrapped.fixed).getOrElse(<td align="right"></td>)

    def tryCast(value: String): Try[Option[A]] = Option(value.trim).filterNot(_.isEmpty) match {
      case Some(v) ⇒ wrapped.tryCast(value).map(Some(_))
      case None    ⇒ Success(None)
    }
  }

  implicit val booleanCell = new ValueCell[Boolean] {
    private def checked(elem: xml.Elem, checked: Boolean) =
      if (checked) elem % xml.Attribute("checked", Seq(xml.Text("checked")), xml.Null) else elem

    def link(base: String, e: Boolean) = fixed(e)
    def editable(e: Boolean)           = <td>{checked(<input type="checkbox"/>, e)}</td>
    def fixed(e: Boolean)              = <td>{checked(<input type="checkbox" disabled="disabled"/>, e)}</td>
    def cast(value: String)            = value.toBoolean
  }

  implicit lazy val doubleCell = Cell[Double](_.toString, _.toDouble)
  implicit lazy val intCell    = Cell[Int]   (_.toString, _.toInt)
  implicit lazy val longCell   = Cell[Long]  (_.toString, _.toLong)
  implicit lazy val stringCell = Cell[String](identity,   identity)
}