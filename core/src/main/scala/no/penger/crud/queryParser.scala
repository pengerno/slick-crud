package no.penger
package crud

import scala.slick.lifted.TableQuery

trait queryParser {

  object QueryParser {
    import scala.slick.ast._

    case class IndexedColumn(ref: Symbol, idx: Int)

    /* naive depth first search into a query */
    object Dfs{
      def find[T](pf: PartialFunction[Node, T])(under: Node): Option[T] =
        Some(under) collectFirst pf orElse
          (under.nodeChildren map find(pf) collectFirst { case Some(found) ⇒ found})

      def get[T](pf: PartialFunction[Node, T])(under: Node): T = find(pf)(under).get
    }

    object columnNames extends (Q ⇒ Seq[ColumnName]){

      def apply(q: Q): Seq[ColumnName] = colsFromQuery(q.toNode)

      /* recursively lookup column references into the tree until we find a TableExpansion with names */
      def colsFromQuery(cols: Node): Seq[ColumnName] = cols match {
        /* return column names */
        case TableExpansion(_, _, colNames) ⇒ columnsFor(colNames)

        /* mostly ignore joins, pick out the original table */
        case Bind(_, Join(_, _, left, _, _, _), _) ⇒
          colsFromQuery(left)

        /* select a subset of the columns (that we iterate further to find definitions for) */
        case Bind(_, from, selects) ⇒ selectFrom(selects, colsFromQuery(from))

        /* basically ignore these
            im sure there will be other that will show up here, so we should be more clever,
            for example by DFS of the children for things we know
        */
        case SortBy(_, from, _) ⇒ colsFromQuery(from)
        case Filter(_, from, _) ⇒ colsFromQuery(from)
      }

      /* picks subset of columns */
      def selectFrom(selects: Node, cols: Seq[ColumnName]): Seq[ColumnName] =
        columnsOrIndexFor(selects).foldLeft[Seq[ColumnName]](Seq.empty){
          case (acc, Left(IndexedColumn(_, idx))) ⇒ acc :+ cols(idx - 1)
          case (acc, Right(name))                 ⇒ acc :+ name
        }

      /* find all column names or column references under node  */
      def columnsOrIndexFor(n: Node) = {
        object NamedOrIndexedColumn {
          def unapply(n: Node): Option[Either[IndexedColumn, ColumnName]] = n match {
            case Select(_, FieldSymbol(name))         ⇒ Some(Right(ColumnName(name)))
            case Select(Ref(sym), ElementSymbol(idx)) ⇒ Some(Left(IndexedColumn(sym, idx)))
            case _ ⇒ None
          }
        }

        Dfs.get[Seq[Either[IndexedColumn, ColumnName]]] {
          /* more than one column selected */
          case ProductNode(cs) ⇒ cs map {
            /* normal column */
            case NamedOrIndexedColumn(name) ⇒ name
            /* optional column */
            case OptionApply(NamedOrIndexedColumn(name)) ⇒ name
          }
          /* exactly one column selected */
          case NamedOrIndexedColumn(name) ⇒ Seq(name)
        }(n)
      }

      /* find all column names under node  */
      def columnsFor(n: Node): Seq[ColumnName] = {
        object NamedColumn {
          def unapply(n: Node) = n match {
            case Select(_, FieldSymbol(name)) ⇒ Some(ColumnName(name))
            case _ ⇒ None
          }
        }

        Dfs.get[Seq[ColumnName]] {
          /* more than one column selected */
          case ProductNode(cs) ⇒ cs map {
            /* normal column */
            case NamedColumn(name) ⇒ name
            /* optional column */
            case OptionApply(NamedColumn(name)) ⇒ name
          }
          /* exactly one column selected */
          case NamedColumn(name) ⇒ Seq(name)
        }(n)
      }
    }

    def tableNameFrom(table: TableQuery[_]): TableName = table.toNode match {
      case TableExpansion(_, TableNode(_, tableName, _, _, _), _) => TableName(tableName)
    }
  }
}