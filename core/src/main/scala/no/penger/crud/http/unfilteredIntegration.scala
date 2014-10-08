package no.penger.crud
package http

import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request._
import unfiltered.response._

trait unfilteredIntegration extends editorAbstracts {

  def respond(ctx: String, title: String)(body: PageFormat): ResponseFunction[Any]

  case class EditorUnfiltered[ID](editor: EditorAbstract[ID]){

    val MountedAt: List[String] = Seg.unapply(editor.mounted).get

    /* extract id from url */
    object Id {
      def unapply(parts: String): Option[ID] = editor.idCell.tryCast(parts.split("/").head).toOption
    }

    /* extract column updates */
    object ColUpdates {
      def unapply[T](req: HttpRequest[T]) =
        Some(req.parameterNames.foldLeft[Map[ColumnName, String]](Map.empty)((acc, n) ⇒
          acc + (ColumnName(n) → req.parameterValues(n).head)
        ))
    }

    def intent:Plan.Intent = {

      case req@GET(ContextPath(ctx, Seg(MountedAt))) ⇒
        respond(ctx, title = MountedAt.head)(editor.view(ctx))

      case req@GET(ContextPath(ctx, Seg(MountedAt :+ "new"))) ⇒
        respond(ctx, title = s"new ${editor.tableName}") (
          editor.viewNew(ctx)
        )

      case req@POST(ContextPath(ctx, Seg(MountedAt :+ "new"))) & ColUpdates(params) ⇒
        editor.create(params) match {
          case Left(fails) ⇒
            BadRequest ~> ResponseString(fails.mkString("\n"))
          case Right(Some(id)) ⇒
            respond(ctx, s"created new ${editor.tableName}")(editor.viewRow(ctx, id))
          case Right(None) ⇒
            respond(ctx, s"created new ${editor.tableName}")(editor.view(ctx))
        }

      case req@GET(ContextPath(ctx, Seg(MountedAt :+ Id(id)))) ⇒
        respond(ctx, title = s"${editor.tableName} for $id") (
          editor.viewRow(ctx, id)
        )

      case req@POST(ContextPath(_, Seg(MountedAt :+ Id(id)))) & ColUpdates(updates) ⇒
        editor.update(id, updates) match {
          case Left(fails: Seq[FailedUpdate]) ⇒
            BadRequest ~> ResponseString(fails.mkString("\n"))
          case Right(okUpdates) ⇒
            Ok ~> ResponseString(okUpdates.mkString("\n"))
        }
    }
  }

  trait CrudPlan extends Plan {
    def editors: Seq[EditorAbstract[_]]
    def resourceIntent: Plan.Intent
    lazy val editorIntents = editors.map(new EditorUnfiltered(_)).map(_.intent).toSeq
    override lazy val intent = (editorIntents :+ resourceIntent).reduce(_ orElse _)
  }
}
