package no.penger

package object crud {
  trait Crud extends editors with editableInstances with editableProductInstances with viewHtml

  case class Update(column: String, oldValue: Option[Any], newValue: Any, numUpdated: Int)

  case class FailedUpdate(column: String, values: Seq[String], t: Throwable)

  case class ColumnName(asString: String) extends AnyVal{
    override def toString: String = asString
  }
  case class TableName(asString: String) extends AnyVal {
    override def toString: String = asString
    def withColumn(c: ColumnName) = TableColumn(this, c)
  }
  case class TableColumn(t: TableName, c: ColumnName){
    override def toString: String = c.asString
    def columnName = c.asString
  }
}
