package no.penger.crud

import java.util.UUID

import scala.xml.NodeSeq

trait viewHtml extends view with viewFormatHtml {

  /* context path */
  val ctx: String

  override def append(one: NodeSeq, two: NodeSeq) =
    one ++ two

  override def View[ID: Cell, ROW](base: String, tableName: TableName, isEditable: Boolean, id: ColumnName, namedCells: NamedCellRow[ROW]): View[ID, ROW] =
    ViewHtml(ctx + base, tableName, isEditable, id, namedCells)

  case class ViewHtml[ID: Cell, ROW](
    base:       String,
    tableName:  TableName,
    isEditable: Boolean,
    idCol:      ColumnName,
    namedCells: NamedCellRow[ROW]) extends View[ID, ROW] {

    /* generate a random id for the table we render, for frontend to distinguish multiple tables */
    val uniqueId    = tableName+UUID.randomUUID().toString.filter(_.isLetterOrDigit)

    override def renderCell(columnName: ColumnName, value: Any, cell: Cell[Any]): ElemFormat =
      if (idCol =:= columnName)
        <td><a class="btn btn-default btn-s" href={base + "/" + cell.toStr(value)} role="button">{cell.toStr(value)}</a></td>
      else
        <td>{
          checkedCheckbox(cell.inputType == "checkbox" && value == true)( //todo: come up with a way to know if the value corresponds to checked
            enabled(isEditable && cell.isEditable)(
              <input
                type={cell.inputType}
                class={"form-control"}
                align={if (cell.alignRight) "right" else "left"}
                value={cell.toStr(value)}
                placeholder={cell.typeName}
              ></input>
            )
          )
        }</td>

    override def many(rows: Seq[(ID, ROW)]) = {
      <div class="panel panel-primary">
        {header(showNew = true, showSeeAll = true)}
        <script type="text/javascript">no.penger.crud.view('{base}', '#{uniqueId}')</script>
        <table id={uniqueId} class="table" style="width:auto">
          <thead>
            <tr>{namedCells.colNames.map(name ⇒ <th>{name}</th>)} </tr>
          </thead>{
            rows.map {
              case (id, row) ⇒ <tr class="form-group" db-id={Cell.toStr(id)}>{
                namedCells.cellsWithUnpackedValues(row).map {
                  case ((colName, cell), value) ⇒ renderCell(colName, value, cell)
                }
              }</tr>
            }
          }</table>
      </div>
    }

    override def notFound(idOpt: Option[ID]) =
      idOpt match {
        case Some(id) ⇒ <h3>{s"Found no referenced $tableName for id ${Cell.toStr(id)}"}</h3>
        case None     ⇒ <h3>{s"Found no referenced $tableName"}</h3>
      }

    override def single(id: ID, row: ROW) = {
      <div class="panel panel-primary">
        {header(showNew = true, showSeeAll = true)}
        <script type="text/javascript">{s"no.penger.crud.single('$base', '#$uniqueId')"}</script>
        <table id={uniqueId} db-id={Cell.toStr(id)} class="table table-bordered table-hover table-condensed">
          <thead><tr><th>Column</th><th>Value</th></tr></thead>
          {namedCells.cellsWithUnpackedValues(row).map{
            case ((name, cell), value) ⇒ <tr><td>{name}</td>{renderCell(name, value, cell)
          }</tr>}}
        </table>
      </div>
    }

    override def newPage = {
      <div class="panel panel-primary">
        {header(showNew = false, showSeeAll = true)}
        <script type="text/javascript">{s"no.penger.crud.neew('$base', '#$uniqueId')"}</script>
        <table id={uniqueId} class="table table-bordered table-hover">
          <thead><tr><th>Column</th><th>Value</th></tr></thead>{
            namedCells.cells.map{
              case (name, cell) ⇒ <tr><td>{name}</td>{renderEmptyCell(cell)}</tr>
            }
          }
        </table>
        <button id="submit" type="submit" class="btn btn-default">Save</button>
      </div>
    }

    def renderEmptyCell(cell: Cell[Any]): ElemFormat =
      <td>
        <input
          type={cell.inputType}
          class={"form-control"}
          placeholder={cell.typeName}
          ></input>
      </td>

    def header(showNew: Boolean, showSeeAll: Boolean) = {
      <div class="panel-heading">
        {tableName}
        {if (showNew)    <a class="btn btn-default btn-xs" href={base + "/new"} role="button">New</a>     else NodeSeq.Empty}
        {if (showSeeAll) <a class="btn btn-default btn-xs" href={base}          role="button">See all</a> else NodeSeq.Empty}
      </div>
    }

    def enabled(isEnabled: Boolean)(elem: xml.Elem) =
      if (isEnabled) elem else elem % xml.Attribute("disabled", Seq(xml.Text("")), xml.Null)

    def checkedCheckbox(isChecked: Boolean)(elem: xml.Elem) =
      if (isChecked) elem % xml.Attribute("checked", Seq(xml.Text("")), xml.Null) else elem
  }
}
