package no.penger.crud

trait renderFormatHtml extends renderFormat {
  override final type ElemFormat = xml.Elem
  override final type PageFormat = xml.NodeSeq
}
