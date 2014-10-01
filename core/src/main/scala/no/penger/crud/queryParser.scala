package no.penger
package crud

trait queryParser {
  import scala.language.higherKinds

  /* slick integration */
  val profile: slick.driver.JdbcDriver
  def db: profile.simple.Database

  /* aliases */
  final type AbstractTable[T]                  = slick.lifted.AbstractTable[T]
  final type TableQuery[E <: AbstractTable[_]] = slick.lifted.TableQuery[E]
  final type Query[+E, U, C[_]]                = profile.simple.Query[E, U, C]
  final type Q                                 = Query[_, _, Seq]

  /**
   * This is all about diving into slicks AST. Dfs implements a naive depth first search which
   * returns the first match, and most of the code ignores the inherent structure in the tree
   * and picks out what it's looking for. I'm sure that is not always correct.
   */
  object QueryParser {

    import scala.slick.ast.{Bind, ElementSymbol, FieldSymbol, Join, Node, OptionApply, ProductNode, Pure, Select, TableNode}

    trait Dfs[T] {
      def pred: PartialFunction[Node, T]

      /* im sure this will blow up one day. sorry*/
      final def apply(q: Q): T = find(q.toNode).get

      final def apply(under: Node): T = find(under).get

      final def find(under: Node): Option[T] =
        Some(under) collectFirst pred orElse
          (under.nodeChildren.toStream map find collectFirst { case Some(found) => found})
    }

    /* find tablename in a slick ast */
    object tableNameFrom extends Dfs[TableName] {
      override val pred: PartialFunction[Node, TableName] = {
        case TableNode(_, tablename, _, _, _) => TableName(tablename)
      }
    }

    object NamedColumn {
      def unapply(n: Node) = n match {
        case Select(_, FieldSymbol(name)) => Some(ColumnName(name))
        case Select(_, ElementSymbol(idx)) => Some(ColumnName(idx.toString))
        case _ => None
      }
    }

    object columns extends (Q => Seq[TableColumn]) {
      def apply(q: Q): Seq[TableColumn] =
        columnsPerTable(q).flatMap {
          case (table, columns) => columns map table.withColumn
        }.toSeq

      def columnsPerTable(q: Q): Seq[(TableName, Seq[ColumnName])] =
        joinsFor find q.toNode getOrElse Seq(q.toNode) map {
          table => (tableNameFrom(table), columnsFor(bindFor find table getOrElse table))
        }

      object pureFor extends Dfs[Node] {
        override val pred: PartialFunction[Node, Node] = {
          case p: Pure => p
        }
      }

      object bindFor extends Dfs[Node] {
        override val pred: PartialFunction[Node, Node] = {
          case Bind(_, _, n) => n
        }
      }

      /* find names of columns under a given node */
      object columnsFor extends Dfs[Seq[ColumnName]] {
        override val pred: PartialFunction[Node, Seq[ColumnName]] = {
          /* more than one column selected */
          case ProductNode(cs) => cs map {
            /* normal column */
            case NamedColumn(name) => name
            /* optional column */
            case OptionApply(NamedColumn(name)) => name
          }
          /* exactly one column selected */
          case NamedColumn(name) => Seq(name)
        }
      }

      /* this supports nested, ie more than two tables, joins. not tested much, heh */
      object joinsFor extends Dfs[Seq[Node]] {
        def recJoin(n: Node) = joinsFor find n getOrElse Seq(n)

        override val pred: PartialFunction[Node, Seq[Node]] = {
          case Join(_, _, left, right, _, _) => recJoin(left) ++ recJoin(right)
        }
      }
    }

    def primaryKeys(q: Q): Set[TableColumn] = columns(q).toSet
  }
}