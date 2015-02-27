package no.penger.crud

trait urls {
  val ctx: String

  trait EditorUrls {
    val mountedAt: String

    object url {
      val CtxRoot    = linx.Root / ctx
      val Table      = CtxRoot   / mountedAt
      val CreateRow  = Table     / "create"
      val Read       = Table     / "read"
      val ReadPage   = Read      / "page"   / 'page
      val ReadRow    = Read      / "row"    / 'row
      val UpdateRow  = Table     / "update" / 'row
      val DeleteRow  = Table     / "delete" / 'row
    }
  }
}
