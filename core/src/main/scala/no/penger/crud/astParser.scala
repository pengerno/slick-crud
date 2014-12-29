package no.penger
package crud

import scala.slick.ast._
import scala.slick.lifted.{Column, Query, TableQuery}

trait astParser {

  object AstParser {
    def colName(c: Column[Any])       = search.forNamedColumns(c.toNode).head
    def colNames(q: Query[_, _, Seq]) = parse.forNamedColumns(q.toNode)

    def tableName(t: TableQuery[_])   = t.toNode match {
      case TableExpansion(_, TableNode(_, tableName, _, _, _), _) => TableName(tableName)
    }

    /**
     * Quick implementation note:
     * 'parse' is supposed to know how to handle the outer parts of an AST, and hence
     *   know what it's doing. 'search' is dumb (probably just laziness on my part).
     *  Apart from that there is no real semantic difference between them.
     */

    case class IndexedColumn(ref: Symbol, idx: Int)

    object parse {
      /* recursively lookup column references into the tree until we find a TableExpansion with names */
      def forNamedColumns(cols: Node): Seq[ColumnName] = cols match {
        /* return column names */
        case TableExpansion(_, _, colNames) ⇒ search.forNamedColumns(colNames)

        /* mostly ignore joins, pick out the original table */
        case Bind(_, Join(_, _, left, _, _, _), _) ⇒ forNamedColumns(left)
        case Join(_, _, left, _, _, _) ⇒ forNamedColumns(left)

        /* select a subset of the columns (recurse further to find the definitions) */
        case Bind(_, from, selects) ⇒ selectFrom(selects, forNamedColumns(from))

        /* basically ignore these.
            I'm sure there will be other that will show up here, so we should cover more cases
        */
        case SortBy(_, from, _) ⇒ forNamedColumns(from)
        case Filter(_, from, _) ⇒ forNamedColumns(from)
      }

      /* picks the subset represented by 'selects' of 'cols' */
      def selectFrom(selects: Node, cols: Seq[ColumnName]): Seq[ColumnName] =
        search.forNamedOrIndexedColumns(selects).foldLeft(Seq.empty[ColumnName]){
          case (acc, Left(IndexedColumn(_, idx))) ⇒ acc :+ cols(idx - 1)
          case (acc, Right(name))                 ⇒ acc :+ name
        }
    }

    object search {
      val forNamedOrIndexedColumns = searchFor(NamedOrIndexedColumnResult) _
      val forNamedColumns          = searchFor(NamedColumnResult) _

      /* Use 'Dfs' to find the nodes requested by 'SearchResult' at an arbitrary depth */
      def searchFor[T](Ex: SearchResult[T])(under: Node): Seq[T] =
        Dfs.get[Seq[T]](under) {
          /* more than one column selected */
          case ProductNode(cs) ⇒ cs flatMap {
            /* normal column */
            case Ex(name) ⇒ Seq(name)
            /* optional column */
            case OptionApply(Ex(name)) ⇒ Seq(name)
            /* nested set of columns */
            case TypeMapping(child, _, _) ⇒ searchFor(Ex)(child)
          }
          /* exactly one column selected */
          case Ex(name) ⇒ Seq(name)
        }

      /* naive depth first search into a node */
      object Dfs{
        def find[T](pf: PartialFunction[Node, T])(under: Node): Option[T] =
          Some(under) collectFirst pf orElse
            (under.nodeChildren map find(pf) collectFirst { case Some(found) ⇒ found})

        def get[T](under: Node)(pf: PartialFunction[Node, T]): T = find(pf)(under).get //YOLO
      }

      trait SearchResult[T]{
        def unapply(n: Node): Option[T]
      }

      object NamedOrIndexedColumnResult extends SearchResult[Either[IndexedColumn, ColumnName]]{
        override def unapply(n: Node) = n match {
          case Select(_, FieldSymbol(name))         ⇒ Some(Right(ColumnName(name)))
          case Select(Ref(sym), ElementSymbol(idx)) ⇒ Some(Left(IndexedColumn(sym, idx)))
          case _                                    ⇒ None
        }
      }
      object NamedColumnResult extends SearchResult[ColumnName]{
        override def unapply(n: Node) = n match {
          case Select(_, FieldSymbol(name)) ⇒ Some(ColumnName(name))
          case _                            ⇒ None
        }
      }
    }
  }
}