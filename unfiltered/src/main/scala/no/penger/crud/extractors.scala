package no.penger.crud

import unfiltered.request.HttpRequest

trait extractors extends cells {

  abstract class Extractors[ID]{
    def idCell: Cell[ID]

    /* extract id from url */
    object Id {
      def unapply(parts: String): Option[ID] = idCell.tryFromStr(parts.split("/").head).toOption
    }

    /* extract column updates */
    object ColUpdates {
      def unapply[T](req: HttpRequest[T]): Some[Map[ColumnName, String]] =
        Some(req.parameterNames.foldLeft[Map[ColumnName, String]](Map.empty)((acc, n) ⇒
          acc + (ColumnName(n) → req.parameterValues(n).head)
        ))
    }
  }
}
