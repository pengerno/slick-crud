package no.penger.crud

import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request._
import unfiltered.response._

trait unfilteredIntegration extends Plan with editors with extractors with resources {

  /* all the editors you want to expose */
  def editors: Seq[EditorAbstract[_]]

  /* how you want to respond to the request, nice for wrapping the output in more html, for example */
  def respond(title: String)(body: PageFormat): ResponseFunction[Any]

  override final lazy val intent = (editors.map(EditorUnfiltered(_).intent) :+ resourceIntent).reduce(_ orElse _)

  case class EditorUnfiltered[ID](editor: EditorAbstract[ID]) extends Extractors[ID] {
    val idCell    = editor.idCell
    val MountedAt = Seg.unapply(editor.mountedAt).get

    def intent:Plan.Intent = {

      case ContextPath(_, FuzzySeg(MountedAt)) ⇒
        respond(title = MountedAt.head)(editor.view)

      case GET(ContextPath(_, FuzzySeg(MountedAt :+ "new"))) ⇒
        respond(title = s"new ${editor.tableName}") (
          editor.viewNew
        )

      case POST(ContextPath(_, FuzzySeg(MountedAt :+ "new"))) & ColUpdates(params) ⇒
        editor.create(params) match {
          case Left(failed)           ⇒ BadRequest ~> ResponseString(failed.ts.mkString("\n"))
          case Right(Created(table, id)) ⇒ respond(s"created new $table")(editor.viewRow(id))
        }

      case GET(ContextPath(_, FuzzySeg(MountedAt :+ Id(id)))) ⇒
        respond(title = s"${editor.tableName} for $id") (
          editor.viewRow(id)
        )

      case POST(ContextPath(_, FuzzySeg(MountedAt :+ Id(id)))) & ColUpdates(updates) ⇒
        updates.headOption match {
          case Some((columnName, value)) =>
            editor.update(id, columnName, value) match {
              case Left(failed) ⇒
                BadRequest ~> ResponseString(failed.toString)
              case Right(u) ⇒
                Ok ~> ResponseString(u.toString)
            }
          case _ => BadRequest ~> ResponseString("No column -> value provided")
        }
    }
  }
}
