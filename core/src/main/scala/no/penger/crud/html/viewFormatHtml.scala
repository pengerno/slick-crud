package no.penger.crud
package html

trait viewFormatHtml extends viewFormat {
  override final type ElemFormat = xml.Elem
  override final type PageFormat = xml.NodeSeq
}
