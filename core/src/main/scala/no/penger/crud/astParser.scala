package no.penger
package crud

import slick.ast._
import slick.lifted.{Query, Rep}
import slick.util.ConstArray

trait astParser extends errors {

  /**
   * Given a query, we need to know which columns the query maps to.
   * We do this by parsing slicks internal ast:
   *
   * Vaguely, the process goes like this:
   *  - Construct a Map[Symbol, Node] which we use to resolve references (parser.subNodesByReference())
   *  - Construct a Seq[Select] (parser.selectFrom()). A Select is a reference to (sets of) column(s)
   *  - For every Select, resolve it to column names in the AST (parser.resolve())
   */
  object AstParser{

    def colName[TABLE <: AbstractTable[_]](t: TABLE, c: Rep[Any]): ColumnInfo =
      c.toNode match {
        case parser.SingleSelect(Select(_, fs@FieldSymbol(colName))) ⇒
          ColumnInfo(TableName(t.tableName), ColumnName(colName), fs.options)
      }

    def colNames(q: Query[_, _, Seq]): Seq[ColumnInfo] =
      parser(q.toNode)

    private object parser {
      def apply(root: Node): Seq[ColumnInfo] = {
        val nodeLookup: Map[Symbol, Node] = subNodesByReference(root).toMap
        selects(nodeLookup)(root)
      }

      def subNodesByReference(n: Node): ConstArray[(Symbol, Node)] = {
        val references: ConstArray[(Symbol, Node)] = n match {
          case t@TableExpansion(gen, _, _)  ⇒ ConstArray((gen, t))
          case defNode: DefNode             ⇒ defNode.generators
          case else_                        ⇒ ConstArray.empty
        }

        references ++ n.children.flatMap(subNodesByReference)
      }

      def selects(nodeLookup: Map[Symbol, Node])(root: Node): Seq[ColumnInfo] =
        selectFrom(root) flatMap resolve(nodeLookup)

      def selectFrom(n: Node): Seq[Select] =
        skipNotInteresting(n) match {
          case TableExpansion(_, _, c)       ⇒ selectFrom(c)
          case Bind(_, _, c)                 ⇒ selectFrom(c)
          case Pure(c, _)                    ⇒ selectFrom(c)
          case TypeMapping(c, _, _)          ⇒ selectFrom(c)
          case Join(_, _, left, right, _, _) ⇒ selectFrom(left) ++ selectFrom(right)
          case pn: ProductNode ⇒
            pn.children.toSeq.collect { case SingleSelect(s) ⇒ s}
          case SingleSelect(s)               ⇒ Seq(s)
        }

      object SingleSelect {
        def unapply(n: Node): Option[Select] =
          n match {
            /* normal column */
            case s: Select                               ⇒ Some(s)
            /* optional column */
            case OptionApply(s: Select)                  ⇒ Some(s)
            /* optional columns as used with joins */
            case OptionFold(from, _, SingleSelect(s), _) ⇒ Some(s)
            /* nested (set of) column(s) */
            case TypeMapping(s: Select, _, _)            ⇒ Some(s)
          }
      }

      def skipNotInteresting(n: Node): Node = n match {
        case s: Select        ⇒ s
        case n: UnaryNode     ⇒ skipNotInteresting(n.child)
        case n: FilteredQuery ⇒ skipNotInteresting(n.from)
        case _                ⇒ n
      }

      def resolve(nodeLookup: Map[Symbol, Node])(currentSelect: Select): Seq[ColumnInfo] = {
        currentSelect match {
          case Select(Ref(ref), FieldSymbol(name)) ⇒
            skipNotInteresting(nodeLookup(ref)) match {
              case TableExpansion(_, TableNode(_, tableName, _, _), colsNode) ⇒
                selectFrom(colsNode).collectFirst {
                  case found@Select(_, fs@FieldSymbol(`name`)) ⇒
                    ColumnInfo(TableName(tableName), ColumnName(name), fs.options)
                }.toSeq
              case inner: Select =>
                resolve(nodeLookup)(inner).find(_.name.toString == name).toSeq
            }
          case Select(Ref(ref), ElementSymbol(oneBasedIdx)) ⇒
            skipNotInteresting(nodeLookup(ref)) match {
              case j: Join ⇒
                selects(nodeLookup)(j.children(oneBasedIdx - 1))
              case b: Bind ⇒
                Seq(selects(nodeLookup)(b)(oneBasedIdx - 1))
              case inner: Select ⇒
                //Seq(resolve(nodeLookup)(inner)(oneBasedIdx - 1))
                resolve(nodeLookup)(inner)
            }
          case Select(inner: Select, FieldSymbol(name)) ⇒
            resolve(nodeLookup)(inner).find(_.name.toString == name).toSeq

          case Select(inner: Select, ElementSymbol(oneBasedIdx)) ⇒
            //Seq(resolve(nodeLookup)(inner)(oneBasedIdx - 1))
            resolve(nodeLookup)(inner)
        }
      }
    }
  }
}