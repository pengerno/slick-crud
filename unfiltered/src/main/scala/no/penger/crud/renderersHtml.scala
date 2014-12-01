package no.penger.crud

import java.util.UUID

import scala.xml.NodeSeq

trait renderersHtml extends renderers with renderFormatHtml {

  implicit class XmlElemX(e: xml.Elem){
    def attachAttr(key: String, value: Option[String]) =
      e % xml.Attribute(key, Seq(xml.Text(value.getOrElse(""))), xml.Null)
    def attachAttrIf(key: String, value: Option[String])(pred: Boolean) =
      if (pred) attachAttr(key, value) else e
    def attachAttrIfNot(key: String, value: Option[String])(pred: Boolean) =
      if (pred) e else attachAttr(key, value)
  }

  /* context path */
  val ctx: String

  override def combine(one: NodeSeq, two: NodeSeq) =
    one ++ two

  override def Renderer[ID: Cell, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]) =
    RendererHtml(ref)

  case class RendererHtml[ID: Cell, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]) extends Renderer[ID, P] {
    val base = ctx + ref.base.mounted

    /* generate a random id for the table we render, for frontend to distinguish multiple tables */
    def newUniqueId = ref.base.tableName+UUID.randomUUID().toString.filter(_.isLetterOrDigit)
    def withId[T](f: String ⇒ T) = f(newUniqueId)

    def innerCell(columnName: ColumnName, value: Any, anyCell: Cell[Any]): ElemFormat =
      anyCell match {
        case PKCell(wrapped) =>
          <a href={base + "/" + anyCell.toStr(value)} class="btn-style">
            {ref.base.tableName + " ("}<strong>
            {anyCell.toStr(value)}
          </strong>{")"}
          </a>

        case fk: FKCell[Any] =>
          /* todo: ugh, refactor! */
          if (fk.wrapped.typeName.contains("FK[") || fk.wrapped.typeName.contains("PK[")){
            innerCell(columnName, value, fk.wrapped)
          } else if (fk.typeName.contains("Option["))
            <select>
              <option value="">None</option>
              {fk.possibleValues.map {
                case `value` => <option selected="selected" value={fk.wrapped.toStr(value)}>{fk.wrapped.toStr(value)}</option>
                case alt     => <option                     value={fk.wrapped.toStr(alt)}>{fk.wrapped.toStr(alt)}</option>
              }
            }</select>
          else
            <select required="required">{
              fk.possibleValues.map {
                case `value` => <option selected="selected" value={fk.wrapped.toStr(value)}>{fk.wrapped.toStr(value)}</option>
                case alt     => <option                     value={fk.wrapped.toStr(alt)}>{fk.wrapped.toStr(alt)}</option>
              }
            }</select>

        case _ if anyCell.inputType == "checkbox" =>
            <input type="checkbox"/>
            .attachAttrIf("checked", None)(value == true || value == Some(true))
        case _ =>
            <input
              class={if (anyCell.alignRight) "right" else "left"}
              type={anyCell.inputType}
              placeholder={anyCell.typeName}
              value={anyCell.toStr(value)}
              autocomplete="off"
            />
      }

    override def cell(columnName: ColumnName, value: Any, anyCell: Cell[Any]) = {
      <td>{innerCell(columnName, value, anyCell)
        .attachAttrIfNot("disabled", None)(ref.base.isEditable && anyCell.isEditable)
        }</td>
    }

    def renderEmptyCell(cell: Cell[Any], valueOpt: Option[String]) = valueOpt match {
      case Some(value) ⇒ <td><input type={cell.inputType} placeholder={cell.typeName} value={value}/></td>
      case _           ⇒ <td><input type={cell.inputType} placeholder={cell.typeName}/></td>
    }

    override def rows[T](rows: Seq[(ID, P)], via: Option[(ColumnName, T)]) = withId {
      uniqueId ⇒
        <div>
          <table id={uniqueId}>
            {header(via, introWord = None, uidShowSave = None, showDelete = None, showNew = true)}
            <thead>
              <tr>{ref.cells.colNames.map(name ⇒
                <th class="columnHeader">{name}</th>)}
              </tr>
            </thead>
            <tbody>{
              rows.zipWithIndex.map {
                case ((id, row), idx) ⇒
                  <tr db-id={Cell.toStr(id)} class={if (idx % 2 == 0) "even" else ""}>{
                    ref.cells.cellsWithUnpackedValues(row).map {
                      case ((colName, c), value) ⇒ cell(colName, value, c)
                    }}
                  </tr>
              }
            }</tbody>
          </table>
          <script type="text/javascript">no.penger.crud.view('{base}', '#{uniqueId}')</script>
        </div>
    }

    override def row[T](id: ID, row: P, via: Option[(ColumnName, T)]) = withId {
      uniqueId ⇒
        <table id={uniqueId} db-id={Cell.toStr(id)}>
          {header(via, introWord = None, uidShowSave = None, showDelete = Some(id), showNew = true)}
          <thead><tr><th>Column</th><th>Value</th></tr></thead>
          {ref.cells.cellsWithUnpackedValues(row).map{
            case ((name, c), value) ⇒ <tr><td class="columnHeader">{name}</td>{cell(name, value, c)}</tr>
          }}
        </table>
        <script type="text/javascript">{s"no.penger.crud.single('$base', '#$uniqueId')"}</script>
    }

    override def createRow[T](via: Option[(ColumnName, T)]) = withId {
      uniqueId ⇒
        <table id={uniqueId}>
          {header(via, introWord = Some("New"), uidShowSave = Some(uniqueId), showDelete = None, showNew = false)}
          <tbody> {
            ref.cells.cells.map{ t => (t, via) match {
              case ((name, cell), Some((colName, value))) if name =:= colName =>
                <tr><th class="columnHeader">{name}</th>{renderEmptyCell(cell, Some(cell.toStr(value)))}</tr>
              case ((name, cell), _) ⇒
                <tr><th class="columnHeader">{name}</th>{renderEmptyCell(cell, None)}</tr>
              }
            }
          }</tbody>
        </table>
        <script type="text/javascript">{s"no.penger.crud.neew('$base', '#$uniqueId')"}</script>
    }

    override def noRow[T](via: Option[(ColumnName, T)]) =
      header(via, introWord = Some("No"), uidShowSave = None, showDelete = None, showNew = false)

    def header[T](via:           Option[(ColumnName, T)],
                  introWord:     Option[String],
                  uidShowSave:   Option[String],
                  showDelete:    Option[ID],
                  showNew:       Boolean) =
      <caption class="columnHeader">
        <strong>{
          (via, introWord) match {
            case (Some((colName, value)), Some(i)) => s"$i ${ref.base.tableName} for $colName = $value"
            case (Some((colName, value)), None)    =>    s"${ref.base.tableName} for $colName = $value"
            case (None,                   Some(i)) => s"$i ${ref.base.tableName}"
            case (None,                   None)    =>        ref.base.tableName
          }}</strong>
        {if (ref.base.isEditable && showNew) <a class="btn-style" href={base + "/new"}>New</a> else NodeSeq.Empty}
        {uidShowSave match {
           case Some(uid) if ref.base.isEditable => <a id={uid + "submit"} class="btn-style" href="#">Save</a>
           case _ => NodeSeq.Empty
        }}
        {showDelete match {
           case Some(id) if ref.base.isEditable =>
             <a class="btn-style" href={base + "/delete/" + implicitly[Cell[ID]].toStr(id)}>Delete</a>
           case _ => NodeSeq.Empty
        }}
        <a class="btn-style" href={base}>See all</a>
      </caption>
  }
}
