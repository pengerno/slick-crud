package no.penger.crud
package http

import unfiltered.filter.Plan
import unfiltered.request._
import unfiltered.response._

trait unfilteredIntegration extends editorAbstracts with extractors {

  def respond(ctx: Ctx, title: String)(body: PageFormat): ResponseFunction[Any]

  case class EditorUnfiltered[ID](editor: EditorAbstract[ID]) extends Extractors[ID] {
    val idCell    = editor.idCell
    val MountedAt = Seg.unapply(editor.mounted).get

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
