package no.penger.crud

import linx.{Root, StaticLinx}

trait urls {
  val ctx: String

  trait EditorUrls {
    val mountedAt: String

    private implicit class MakeLinxLenient(l: StaticLinx){
      def \/(urlFragments: String) =
        urlFragments.split("/").filterNot(_.isEmpty).foldLeft(l){
          case (acc, urlFragment) => acc / urlFragment
        }
    }

    object url {
      val CtxRoot    = Root      \/ ctx
      val Table      = CtxRoot   \/ mountedAt
      val CreateRow  = Table     /  "create"
      val Read       = Table     /  "read"
      val ReadPage   = Read      /  "page"   / 'page
      val ReadRow    = Read      /  "row"    / 'row
      val UpdateRow  = Table     /  "update" / 'row
      val DeleteRow  = Table     /  "delete" / 'row
    }
  }
}
