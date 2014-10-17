package no.penger.crud

trait viewFormatHtml extends viewFormat {
  override final type ElemFormat = xml.Elem
  override final type PageFormat = xml.NodeSeq
}
