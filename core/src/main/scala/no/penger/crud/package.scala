package no.penger

package object crud {
  case class ColumnName(asString: String) extends AnyVal{
    override def toString: String = asString
  }
  case class TableName(asString: String) extends AnyVal {
    override def toString: String = asString
  }
  case class TableColumn(t: TableName, c: ColumnName){
    override def toString: String = c.asString
    def columnName = c.asString
  }
}
