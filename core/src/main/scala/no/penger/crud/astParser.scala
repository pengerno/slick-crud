package no.penger
package crud

import scala.slick.ast._
import scala.slick.lifted.{Column, Query, TableQuery}

trait astParser extends errors {

  /**
   * This class extracts pairs of table and column names from a query.
   * That is a bit harder than i appreciated.
   *
   * In the following implementation, the problem is split in two:
   * `selectedColumns` is the easy part - it extracts references to all selected columns from the outermost layer of a query

   * `resolveSelectedColumns` looks up the references (as such a reference generally does not contain the column name)
   *  elsewhere in the AST. We start by recursing into the deepest/first layer of the query AST, and add/remove
   *  selected columns following the AST. Finally the names are looked up and returned.
   */
  object AstParser{

    def colName[TABLE <: AbstractTable[_]](t: TABLE, c: Column[Any]): ColumnInfo =
      c.toNode match {
        case selectedColumns.SingleSelect(Select(_, fs@FieldSymbol(colName))) ⇒ ColumnInfo(TableName(t.tableName), ColumnName(colName), fs.options)
      }

    def colNames(q: Query[_, _, Seq]): Seq[ColumnInfo] =
      explodeOnError(resolveColumnsUnderNode(q.toNode)).flatMap(_.ns)

    def tableName(t: TableQuery[_]): TableName =
      explodeOnError(TableNameParser(t.toNode) toRight Seq(errorMsg(s"Could not find table name in table query $t")))

    /**
     * These are all non-recoverable errors anyway, so don't think i will bother to thread them through the rest of the application
     */
    private def explodeOnError[R](res: Either[Seq[Error], R]): R = res match {
      case Left(errors) ⇒ sys.error(s"Internal error in slick-crud's AST parser: ${errors.mkString(", ")}")
      case Right(ok)    ⇒ ok
    }

    /* this exists to enforce a border of which layer of `Seq`s can be flattened. We need to
     *  be careful the indices utilized in resolveOne() can reference both a single select or a set of them */
    private[AstParser] case class Group[N](ns: Seq[N])

    private object TableNameParser{
      def apply(n: Node): Option[TableName] = n match {
        case TableNode(_, tableName, _, _, _) ⇒ Some(TableName(tableName))
        case TableExpansion(_, table, _)      ⇒ apply(table)
        case n: FilteredQuery                 ⇒ apply(n.from)
        case n: UnaryNode                     ⇒ apply(n.child)
        case _                                ⇒ None
      }
    }

    private object selectedColumns {
      /** This returns Seq because of joins, think of it as a Group[Select] most of the time */
      def apply(under: Node): Seq[Group[Select]] =
        colsFor(under) map selectFrom

      def colsFor(n: Node): Seq[Node] = n match {
        case TableExpansion(_, _, cs)      ⇒ Seq(cs)
        case Bind(_, _, cs)                ⇒ Seq(cs)
        case Join(_, _, left, right, _, _) ⇒ colsFor(left) ++ colsFor(right)
        case n: FilteredQuery              ⇒ colsFor(n.from)
        case n: UnaryNode                  ⇒ colsFor(n.child)
      }

      def selectFrom(n: Node): Group[Select] =
        n match {
          case SingleSelect(s)               ⇒ Group(Seq(s))
          case Pure(c, _)                    ⇒ selectFrom(c)
          case TypeMapping(c, _, _)          ⇒ selectFrom(c)
          case _: ProductNode | _: UnaryNode ⇒
            Group(n.nodeChildren.collect { case SingleSelect(s) ⇒ s})
        }

      object SingleSelect {
        def unapply(nn: Node): Option[Select] =
          nn match {
            /* normal column */
            case s: Select                    ⇒ Some(s)
            /* optional column */
            case OptionApply(s: Select)       ⇒ Some(s)
            /* nested (set of) column(s) */
            case TypeMapping(s: Select, _, _) ⇒ Some(s)
            case _                            ⇒ None
          }
      }
    }

    /** looks up a node by reference in a sub-tree */
    private object lookupRef {
      def apply(ref: Ref, under: Node): Either[Error, Node] =
        doLookup(ref, under).orError(s"Couldn't find referenced node $ref under $under")

      def doLookup(ref: Ref, under: Node): Option[Node] = {
        val Symbol = ref.sym

        val foundOpt: Option[Node] = under match {
          case defNode: DefNode ⇒
            defNode.nodeGenerators.collectFirst {
              case (Symbol, found) ⇒ found
            }
          case _ ⇒ None
        }
        foundOpt orElse under.nodeChildren.foldLeft[Option[Node]](None){
          case (None, child) ⇒ doLookup(ref, child)
          case (found, _)    ⇒ found
        }
      }
    }

    private object resolveColumnsUnderNode {
      def apply(under: Node): Either[Seq[Error], Seq[Group[ColumnInfo]]] =
        sequence(selectedColumns(under) map resolveMany(under)).left.map(_.flatten)

      def resolveMany(under: Node)(selects: Group[Select]): Either[Seq[Error], Group[ColumnInfo]] = for {
        possiblyResolveds ← sequence(selects.ns map resolveOne(under)).left.map(_.flatten)
        resolveds         ← sequence(possiblyResolveds).left.map(unresolveds ⇒ Seq(errorMsg(s"Couldn't resolve nodes $unresolveds")))
      } yield Group(resolveds.flatMap(_.ns))

      def resolveOne(under: Node)(select: Select): Either[Seq[Error], Either[Node, Group[ColumnInfo]]] = {
        /**
         *  Selects can be nested, and each layer might have a reference to a node somewhere else in a tree,
         *  and it generally filters the set of selected columns referenced by that node.
         *  Recurses down to deepest layer
         */
        val resolvedNode: Either[Seq[Error], Either[Node, Group[ColumnInfo]]] = select match {
          case   Select(r: Ref, _)         ⇒ lookupRef(r, under).biMap(Seq(_), Left(_))
          case   Select(inner: Select, _)  ⇒ resolveOne(under)(inner)
        }

        /** filtering columns and final name resolution (by matching FieldSymbol) */
        resolvedNode.right.flatMap {
          case Left(unresolved) ⇒
            select.field match {
              /**
               * Here we resolve a node to column name(s), and pick the selected column name(s)
               * based on the given index.
               * The confusing thing is that the index can be used to filter on two levels:
               * Either a set of columns, or one column exactly
               */
              case ElementSymbol(oneBasedIdx) ⇒
                val columnGroupsE: Either[Seq[Error], Seq[Group[ColumnInfo]]] = resolveColumnsUnderNode(unresolved)
                columnGroupsE.right.map{
                  case columnGroups ⇒
                    Right(
                      if (columnGroups.size == 1) Group(Seq(columnGroups.head.ns(oneBasedIdx - 1)))
                      else columnGroups(oneBasedIdx - 1)
                    )
                }
              /** This is the actual resolve part, i.e. we are given the column name */
              case fs@FieldSymbol(colName) ⇒
                TableNameParser(unresolved) match {
                  case Some(tableName) ⇒ Right(Right(Group(Seq(ColumnInfo(tableName, ColumnName(colName), fs.options)))))
                  case None            ⇒ Left(Seq(errorMsg(s"Couldn't find tableName under $unresolved")))
                }

              case any ⇒ Left(Seq(errorMsg(s"Unhandled select field $any filter while resolving $unresolved")))
            }
          case Right(resolved) ⇒
            select.field match {
              /** filter out columns that were resolved earlier, but which is not selected at this layer */
              case FieldSymbol(colName) ⇒
                Right(Right(Group(resolved.ns.filter(_.name.toString =:= colName))))
              case any ⇒ Left(Seq(errorMsg(s"Unhandled select field $any filter after having resolved $resolved")))
            }
        }
      }
    }
  }
}