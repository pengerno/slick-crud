package no.penger.crud

import java.util.UUID

import scala.xml.NodeSeq

trait renderersHtml extends renderers with viewFormatHtml {

  /* context path */
  val ctx: String

  override def combine(one: NodeSeq, two: NodeSeq) =
    one ++ two

  override def Renderer[ID: Cell, TABLE <: AbstractTable[_], LP, P]
                       (ref: TableRef[ID, TABLE, LP, P]) =
    RendererHtml(ref)

  case class RendererHtml[ID: Cell, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]) extends Renderer[ID, P] {
    val base = ctx + ref.base.mounted

    /* generate a random id for the table we render, for frontend to distinguish multiple tables */
    def newUniqueId = ref.base.tableName+UUID.randomUUID().toString.filter(_.isLetterOrDigit)
    def withId[T](f: String ⇒ T) = f(newUniqueId)

    override def cell(columnName: ColumnName, value: Any, cell: Cell[Any]): ElemFormat =
      <td>{
        if (ref.base.primaryKey =:= columnName)
          <a href={base + "/" + cell.toStr(value)} class="btn-style">{cell.toStr(value)}</a>
        else if (cell.inputType == "checkbox")
          enabled(ref.base.isEditable && cell.isEditable)(
            checkedCheckbox(cell.inputType == "checkbox" && (value == true || value == Some(true)))(
              <input type="checkbox"/>
            )
          )
        else
          enabled(ref.base.isEditable && cell.isEditable)(<input
              class={if (cell.alignRight) "right" else "left"}
              type={cell.inputType}
              placeholder={cell.typeName}
              value={cell.toStr(value)}
              autocomplete="off"
            />)
          }
      </td>

    override def rows(rows: Seq[(ID, P)]) = withId{
      uniqueId ⇒
        <div>
          <table id={uniqueId}>
            {header}
            <thead>
              <tr>{ref.cells.colNames.map(name ⇒ <th class="columnHeader">{name}</th>)} </tr>
            </thead><tbody>{
              rows.zipWithIndex.map {
                case ((id, row), idx) ⇒ <tr db-id={Cell.toStr(id)} class={if (idx % 2 == 0) "even" else ""}>{
                  ref.cells.cellsWithUnpackedValues(row).map {
                    case ((colName, c), value) ⇒ cell(colName, value, c)
                  }
                }</tr>
              }
            }</tbody></table>
          <script type="text/javascript">no.penger.crud.view('{base}', '#{uniqueId}')</script>
        </div>
    }

    override def row(id: ID, row: P) = withId {
      uniqueId ⇒
        <table id={uniqueId} db-id={Cell.toStr(id)}>
          {header}
          <thead><tr><th>Column</th><th>Value</th></tr></thead>
          {ref.cells.cellsWithUnpackedValues(row).map{
            case ((name, c), value) ⇒ <tr><td class="columnHeader">{name}</td>{cell(name, value, c)
          }</tr>}}
        </table>
        <script type="text/javascript">{s"no.penger.crud.single('$base', '#$uniqueId')"}</script>
    }

    override def newRow[T](knownColumn: Option[(ColumnName, T)]) = withId {
      uniqueId ⇒
        <table id={uniqueId}>
          <caption class="columnHeader">
            <strong>{
              knownColumn match {
                case Some((colName, value)) => s"New row for $colName = $value for ${ref.base.tableName}"
                case _                      => s"New row for ${ref.base.tableName}"
              }}</strong>
            <a             class="btn-style" href={base} >See all</a>
            <a id={uniqueId + "submit"} class="btn-style" href="#"    >Save</a>
          </caption>
          <tbody> {
            ref.cells.cells.map{ t => (t, knownColumn) match {
              case ((name, cell), Some((colName, value))) if name =:= colName =>
                <tr><th class="columnHeader">{name}</th>{renderEmptyCellWithValue(cell, value)}</tr>
              case ((name, cell), _) ⇒
                <tr><th class="columnHeader">{name}</th>{renderEmptyCell(cell)}</tr>
              }
            }
          }</tbody>
        </table>
        <script type="text/javascript">{s"no.penger.crud.neew('$base', '#$uniqueId')"}</script>
    }
    //todo: refactor
    def renderEmptyCell(cell: Cell[Any]): ElemFormat =
      <td>
        <input
          type={cell.inputType}
          class={"form-control"}
          placeholder={cell.typeName}
          ></input>
      </td>

    def renderEmptyCellWithValue(cell: Cell[Any], value: Any): ElemFormat =
      <td>
        <input
          type={cell.inputType}
          class={"form-control"}
          placeholder={cell.typeName}
          value={cell.toStr(value)}
          ></input>
      </td>

    def header =
      <caption class="columnHeader">
        <strong>{ref.base.tableName}</strong>
        {if (ref.base.isEditable) <a class="btn-style" href={base + "/new"} >New</a> else NodeSeq.Empty}
        <a class="btn-style" href={base}          >See all</a>
      </caption>

    def checkedCheckbox(isChecked: Boolean)(elem: xml.Elem) =
      if (isChecked) elem % xml.Attribute("checked", Seq(xml.Text("")), xml.Null) else elem

    def enabled(isEnabled: Boolean)(elem: xml.Elem) =
      if (isEnabled) elem else elem % xml.Attribute("disabled", Seq(xml.Text("")), xml.Null)
  }
}
