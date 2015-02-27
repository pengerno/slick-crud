package no.penger.crud

trait positions {
  sealed trait Position{
    def nextPage: Option[Int] = None
    def prevPage: Option[Int] = None
  }
  case class PagedPosition(startRow: Int, endRow: Int, totalRows: Int, page: Int) extends Position{
    override def nextPage = Some(page + 1).filterNot(_ ⇒ endRow >= totalRows)
    override def prevPage = Some(page - 1).filterNot(_ ⇒ startRow == 0)
  }
  case object NotPaged extends Position

}
