package no.penger.crud

import java.util.UUID

import scala.xml.NodeSeq

trait renderersHtml extends renderers with renderFormatHtml with urls {

  implicit class XmlElemX(e: xml.Elem){
    def attachAttr(key: String, value: Option[String]) =
      e % xml.Attribute(key, Seq(xml.Text(value.getOrElse(""))), xml.Null)
    def attachAttrIf(key: String, value: Option[String])(pred: Boolean) =
      if (pred) attachAttr(key, value) else e
    def attachAttrIfNot(key: String, value: Option[String])(pred: Boolean) =
      if (pred) e else attachAttr(key, value)
  }

  override def combine(one: NodeSeq, two: NodeSeq) =
    one ++ two

  override def Renderer[ID, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]) =
    RendererHtml(ref)

  case class RendererHtml[ID, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]) extends Renderer[ID, P] with EditorUrls {
    override val mountedAt = ref.base.mounted

    /* generate a random id for the table we render, for frontend to distinguish multiple tables */
    def newUniqueId = ref.metadata.tableName.toString + UUID.randomUUID().toString.filter(_.isLetterOrDigit)
    def withId[T](f: String ⇒ T) = f(newUniqueId)

    /* this is a hack that is needed because if a column has an optional
        foreign key on a non-optional column in another table, we're
        unable to capture with types so far that so far
     */
    def ensureOptional(mustBeOption: Boolean)(a: Any): Any = a match {
      case alreadyOption: Option[Any] ⇒ a
      case notOption if mustBeOption  ⇒ Some(notOption).asInstanceOf[Any]
      case ok                         ⇒ ok
    }

    def innerCell(columnName: ColumnInfo, value: Any, anyCell: Cell[Any], cache: CacheLookup): ElemFormat =
      anyCell match {
        case PKCell(_) =>
          <a href={url.ReadRow(anyCell.toStr(value))} class="btn-style">
            {ref.metadata.tableName.toString + " ("}<strong>{anyCell.toStr(value)}</strong>{")"}
          </a>

        case b: BooleanCell[_] ⇒
          if (b.isTrue(value)) <input type="checkbox" checked="checked"/>
          else                 <input type="checkbox"/>

        case c if c.constrainedValues.isDefined ⇒
          <select>
            {if (c.isOptional) <option value=""/> else NodeSeq.Empty}
            {
            c.constrainedValues.get(cache).map(ensureOptional(c.isOptional)).map {
              case alt@`value`       => <option selected="selected" value={c.toStr(alt)}>{c.toStr(alt)}</option>
              case alt               => <option                     value={c.toStr(alt)}>{c.toStr(alt)}</option>
            }
            }</select>

        case c => <input type="text" placeholder={c.typeName} value={c.toStr(value)} autocomplete="off"/>
      }

    def cell(mainTable: TableName, columnName: ColumnInfo, value: Any, anyCell: Cell[Any], cache: CacheLookup, rowHasId: Boolean) =
      <td>{
        innerCell(columnName, value, anyCell, cache)
          .attachAttrIfNot("disabled", None)(ref.base.isEditable && anyCell.isEditable && rowHasId && mainTable =:= columnName.table)
        }</td>

    def renderEmptyCell(cell: Cell[Any], valueOpt: Option[String]) = valueOpt match {
      case Some(value) ⇒ <td><input type="text" placeholder={cell.typeName} value={value}/></td>
      case _           ⇒ <td><input type="type" placeholder={cell.typeName}/></td>
    }

    override def message(s: String) = <h2>{s}</h2>

    override def rows[T](mainTable: TableName, isLinked: Boolean, pos: Position, rows: Seq[(Option[ID], P)], via: Option[(ColumnInfo, T)]) = withId {
      uniqueId ⇒
        val cache = new CacheLookup
        <div>
          <table id={uniqueId}>
            {header(via, introWord = None, uidShowSave = None, showDelete = None, showNew = true, showSeeAll = isLinked, positionOpt = Some(pos))}
            <thead>
              <tr>{ref.metadata.colNames.map(name ⇒
                <th class="columnHeader">{name}</th>)}
              </tr>
            </thead>
            <tbody>{
              rows.zipWithIndex.map {
                case ((idOpt, row), idx) ⇒
                  <tr db-id={idOpt.fold("missing")(ref.metadata.idCell.toStr)} class={if (idx % 2 == 0) "even" else ""}>{
                    ref.metadata.cellsWithUnpackedValues(row).map {
                      case ((colName, c), value) ⇒ cell(mainTable, colName, value, c, cache, idOpt.isDefined)
                    }}
                  </tr>
              }
            }</tbody>
          </table>
          <script type="text/javascript">no.penger.crud.view('{url.Table()}', '#{uniqueId}')</script>
        </div>
    }

    override def row[T](mainTable: TableName, idOpt: Option[ID], canDelete: Boolean, row: P, via: Option[(ColumnInfo, T)]) = withId {
      uniqueId ⇒
        val cache = new CacheLookup

        <table id={uniqueId} db-id={idOpt.fold("missing")(ref.metadata.idCell.toStr)}>
          {header(via, introWord = None, uidShowSave = None, showDelete = idOpt.filter(_ => canDelete), showNew = true, showSeeAll = true, positionOpt = None)}
          <thead><tr><th>Column</th><th>Value</th></tr></thead>
          {ref.metadata.cellsWithUnpackedValues(row).map{
            case ((name, c), value) ⇒ <tr><td class="columnHeader">{name}</td>{cell(mainTable, name, value, c, cache, idOpt.isDefined)}</tr>
          }}
        </table>
        <script type="text/javascript">{s"no.penger.crud.single('${url.Table()}', '#$uniqueId')"}</script>
    }

    override def createRow[T](via: Option[(ColumnInfo, Option[T])]) = withId {
      uniqueId ⇒
        <table id={uniqueId}>
          {header(via, introWord = Some("Create new"), uidShowSave = Some(uniqueId), showDelete = None, showNew = false, showSeeAll = true, positionOpt = None)}
          <tbody> {
            ref.base.metadata.cells.map{ t => (t, via) match {
              case ((name, cell), Some((colName, Some(value)))) if name =:= colName =>
                <tr><th class="columnHeader">{name}</th>{renderEmptyCell(cell, Some(cell.toStr(value)))}</tr>
              case ((name, cell), _) ⇒
                <tr><th class="columnHeader">{name}</th>{renderEmptyCell(cell, None)}</tr>
              }
            }
          }</tbody>
        </table>
        <script type="text/javascript">{s"no.penger.crud.neew('${url.Table()}', '#$uniqueId')"}</script>
    }

    override def noRow[T](via: Option[(ColumnInfo, Option[T])]) =
      header(via, introWord = Some("No"), uidShowSave = None, showDelete = None, showNew = false, showSeeAll = false, positionOpt = None)

    def header[T](via:           Option[(ColumnInfo, T)],
                  introWord:     Option[String],
                  uidShowSave:   Option[String],
                  showDelete:    Option[ID],
                  showNew:       Boolean,
                  showSeeAll:    Boolean,
                  positionOpt:   Option[Position]) = {
      <caption class="columnHeader">
        <strong>{
          val posStringOpt = positionOpt collect {
            case PagedPosition(start, end, total, page) => s"($start to $end of $total)"
          }
          val tableString = (via, introWord) match {
            case (Some((colName, value)), Some(i)) => s"$i ${ref.metadata.tableName} for $colName = $value"
            case (Some((colName, value)), None)    =>    s"${ref.metadata.tableName} for $colName = $value"
            case (None,                   Some(i)) => s"$i ${ref.metadata.tableName}"
            case (None,                   None)    =>        ref.metadata.tableName.toString
          }
          posStringOpt.fold(tableString)(posString => tableString + " " + posString)
        }</strong>
        {if (ref.base.isEditable && showNew) <a class="btn-style" href={url.CreateRow()}>New</a> else NodeSeq.Empty}
        {uidShowSave match {
           case Some(uid) if ref.base.isEditable => <a id={uid + "submit"} class="btn-style" href="#">Save</a>
           case _ => NodeSeq.Empty
        }}
        {showDelete match {
           case Some(id) if ref.base.isEditable =>
             <a class="btn-style delete" href="#">Delete</a>
           case _ => NodeSeq.Empty
        }}
        {if (showSeeAll) <a class="btn-style" href={url.Read()}>See all</a> else NodeSeq.Empty}
        {positionOpt.flatMap(_.prevPage) match {
          case Some(prevPage) => <a class="btn-style" href={url.ReadPage(prevPage.toString)}>Previous page</a>
          case None => NodeSeq.Empty
        }}
        {positionOpt.flatMap(_.nextPage) match {
          case Some(nextPage) => <a class="btn-style" href={url.ReadPage(nextPage.toString)}>Next page</a>
          case _              => NodeSeq.Empty
        }}
      </caption>
    }
  }
}
