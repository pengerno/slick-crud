package no.penger.crud

trait testCells extends cells with testView {

  /* handling of optional values*/
  implicit def stringOptionCell[A](implicit wrapped: Cell[A]): Cell[Option[A]] = new Cell[Option[A]] {
    def link(ctx: String, oe: Option[A]) = oe.map(e ⇒ wrapped.link(ctx, e)).getOrElse("")
    def editable(oe: Option[A]) = oe map wrapped.editable getOrElse ""
    def fixed(oe: Option[A]) = oe map wrapped.fixed getOrElse ""

    def tryCast(value: String): util.Try[Option[A]] = Option(value.trim).filterNot(_.isEmpty) match {
      case Some(v) => wrapped.tryCast(value).map(Some(_))
      case None    => util.Success(None)
    }
  }

  def stringCell[E](toStr: E ⇒ String, fromStr: String ⇒ E) = new Cell[E] {
    override def link(ctx: String, e: E) = toStr(e)
    override def editable(e: E) = toStr(e)
    override def tryCast(value: String) = util.Try(fromStr(value))
    override def fixed(e: E) = toStr(e)
  }

}
