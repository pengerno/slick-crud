package no.penger.crud

trait editableInstances extends editables {

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

  /**
   * Use this to use tables mapped to a non-tuple structure.
   **/
  def mappedEditable[Mapped, Tupled <: Product : Editable](unapply: Mapped => Option[Tupled]) =
    new Editable[Mapped] {
      private val wrapped          = implicitly[Editable[Tupled]]
      override def list(e: Mapped) = wrapped.list(unapply(e).get)
      override def cells           = wrapped.cells
    }
}