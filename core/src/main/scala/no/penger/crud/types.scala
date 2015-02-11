package no.penger.crud

import scala.slick.ast.ColumnOption

case class ColumnName(override val toString: String)
case class TableName(override val toString: String)

case class ColumnInfo(table: TableName, name: ColumnName, options: scala.Seq[scala.slick.ast.ColumnOption[_]]) {
  def isAutoIncrement = options contains ColumnOption.AutoInc
  override def toString = name.toString
}