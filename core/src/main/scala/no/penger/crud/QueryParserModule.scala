package no.penger
package crud

trait QueryParserModule extends db.SlickTransactionBoundary {
  import scala.language.higherKinds
  /* aliases */
  final type Query[+E, U, C[_]] = profile.simple.Query[E, U, C]
  final type Q = Query[_, _, Seq]

  /**
   * This is all about diving into slicks AST. Dfs implements a naive depth first search which
   * returns the first match, and most of the code ignores the inherent structure in the tree
   * and picks out what it's looking for. I'm sure that is not always correct.
   */
  object QueryParser {

    import scala.slick.ast.{FieldSymbol, Join, Node, OptionApply, ProductNode, Pure, Select, TableNode}

    trait Dfs[T] {
      def pred: PartialFunction[Node, T]

      /* im sure this will blow up one day. sorry*/
      final def apply(q: Q): T = get(q.toNode).get

      final def apply(under: Node): T = get(under).get

      final def get(under: Node): Option[T] =
        Some(under) collectFirst pred orElse
          (under.nodeChildren.toStream map get collectFirst { case Some(found) => found})
    }

    /* find tablename in a slick ast */
    object tablenameFrom extends Dfs[String] {
      override val pred: PartialFunction[Node, String] = {
        case TableNode(_, tablename, _, _, _) => tablename
      }
    }

    object columns extends (Q => Seq[String]) {
      def apply(q: Q): Seq[String] =
        columnsPerTable(q).flatMap {
          case (table, columns) => columns.map(table + "." + _)
        }.toSeq

      def columnsPerTable(q: Q): Map[String, Seq[String]] =
        joinsFor.get(q.toNode).getOrElse(Seq(q.toNode)).map(
          table => (tablenameFrom(table), columnsFor(pureFor.get(table) getOrElse table))
        ).toMap

      /* this showed up in some queries */
      object pureFor extends Dfs[Node] {
        override val pred: PartialFunction[Node, Node] = {
          case p: Pure => p
        }
      }

      /* find names of columns under a given node */
      object columnsFor extends Dfs[Seq[String]] {

        object NamedColumn {
          def unapply(n: Node) = n match {
            case (Select(_, FieldSymbol(name))) => Some(name)
            case _ => None
          }
        }

        override val pred: PartialFunction[Node, Seq[String]] = {
          /* more than one column selected */
          case ProductNode(cs) => cs map {
            /* normal column */
            case (NamedColumn(name)) => name
            /* optional column */
            case (OptionApply(NamedColumn(name))) => name
          }
          /* exactly one column selected */
          case NamedColumn(name) => Seq(name)
        }
      }

      /* this supports nested, ie more than two tables, joins. not tested much, heh */
      object joinsFor extends Dfs[Seq[Node]] {
        def recJoin(n: Node) = joinsFor.get(n).getOrElse(Seq(n))

        override val pred: PartialFunction[Node, Seq[Node]] = {
          case Join(_, _, left, right, _, _) => recJoin(left) ++ recJoin(right)
        }
      }
    }

    def primaryKeys(q: Q): Set[String] = columns(q).toSet
  }
}