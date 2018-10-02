package no.penger.crud

import slick.ast.ColumnOption

case class ColumnName(override val toString: String)
case class TableName(override val toString: String)

case class ColumnInfo(table: TableName, name: ColumnName, options: scala.Seq[ColumnOption[_]]) {
  def isAutoIncrement = options contains ColumnOption.AutoInc
  override def toString = name.toString
}