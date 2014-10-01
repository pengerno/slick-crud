package no.penger
package crud

trait QueryParserModule extends db.SlickTransactionBoundary {

  object QueryParser {
    import profile.simple._
    import slick.ast._

    private type Q = Query[_, _, Seq]

    trait Dfs[T] {
      def pred: PartialFunction[Node, T]

      /* todo: do some proper error reporting later */
      final def apply(q: Q): T        = get(q.toNode).get
      final def apply(under: Node): T = get(under).get

      final def get(q: Q): Option[T] = get(q.toNode)
      final def get(under: Node): Option[T] =
        Some(under) collectFirst pred orElse
          (under.nodeChildren.toStream map get collectFirst { case Some(found) => found})
    }

    /* find a TableExpansion structure in a slick ast */
    object tableFrom extends Dfs[TableExpansion] {
      override val pred: PartialFunction[Node, TableExpansion] = {
        case te: TableExpansion => te
      }
    }
    
    /* find tablename in a slick ast */
    object tablenameFrom extends Dfs[String] {
      override val pred: PartialFunction[Node, String] = {
        case TableNode(_, tablename, _, _, _) => tablename
      }
    }

    object pureFor extends Dfs[Node] {
      override def pred: PartialFunction[Node, Node] = {
        case p: Pure => p
      }
    }

    /* find names of columns under a given node */
    private object columnsFor extends Dfs[Seq[String]] {
      val pred: PartialFunction[Node, Seq[String]] = {
        case ProductNode(cs) => cs map {
          case (Select(_, FieldSymbol(name))) => name
          /* optional columns */
          case (OptionApply(Select(_, FieldSymbol(name)))) => name
        }
        /* if there is one column selected there is no productnode */
        case Select(_ , FieldSymbol(name)) => Seq(name)
      }
    }

    /* this supports nested, ie more than two tables, joins. not tested much, heh */
    private object joinsFor extends Dfs[Seq[Node]] {
      override def pred: PartialFunction[Node, Seq[Node]] = {
        case Join(_, _, left, right, _, _) => joinsFor.get(left).getOrElse(Seq(left)) ++ joinsFor.get(right).getOrElse(Seq(right))
      }
    }

    private def columnsForTables(q: Q): Map[String, Seq[String]] =
      joinsFor.get(q) match {
        case Some(tables) => tables.map(table => (tablenameFrom(table), columnsFor(pureFor.get(table) getOrElse table))).toMap
        case _            => Map(tablenameFrom(q) -> columnsFor(pureFor.get(q) getOrElse q.toNode))
      }

    def primaryKeys(q: Q): Set[String] = columns(q).toSet
    def columns(q: Q): Seq[String] =
      columnsForTables(q).flatMap {
        case (key, values) => values.map(key + "." + _)
      }.toSeq
  }
}