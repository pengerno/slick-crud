package no.penger.crud

import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request._
import unfiltered.response._

trait unfilteredIntegration extends Plan with editorAbstracts with extractors with resources {

  /* all the editors you want to expose */
  def editors:        Seq[EditorAbstract[_]]

  /* how you want to respond to the request, nice for wrapping the output in more html, for example */
  def respond(title: String)(body: PageFormat): ResponseFunction[Any]

  override final lazy val intent = (editors.map(EditorUnfiltered(_).intent) :+ resourceIntent).reduce(_ orElse _)

  case class EditorUnfiltered[ID](editor: EditorAbstract[ID]) extends Extractors[ID] {
    val idCell    = editor.idCell
    val MountedAt = Seg.unapply(editor.mounted).get

    def intent:Plan.Intent = {

      case req@GET(ContextPath(_, Seg(MountedAt))) ⇒
        respond(title = MountedAt.head)(editor.view)

      case req@GET(ContextPath(_, Seg(MountedAt :+ "new"))) ⇒
        respond(title = s"new ${editor.tableName}") (
          editor.viewNew
        )

      case req@POST(ContextPath(_, Seg(MountedAt :+ "new"))) & ColUpdates(params) ⇒
        editor.create(params) match {
          case Left(fails) ⇒ BadRequest ~> ResponseString(fails.mkString("\n"))
          case Right(id)   ⇒ respond(s"created new ${editor.tableName}")(editor.viewRow(id))
        }

      case req@GET(ContextPath(_, Seg(MountedAt :+ Id(id)))) ⇒
        respond(title = s"${editor.tableName} for $id") (
          editor.viewRow(id)
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
}
