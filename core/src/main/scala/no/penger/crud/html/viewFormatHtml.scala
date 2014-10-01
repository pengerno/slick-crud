package no.penger.crud
package html

trait viewFormatHtml extends viewFormat {
  override final type ViewFormat = xml.NodeSeq
}
