package no.penger.crud

import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request._
import unfiltered.response._

trait unfilteredIntegration extends Plan with editorAbstracts with extractors with resources {

  /* all the editors you want to expose */
  def editors: Seq[EditorAbstract[_]]

  /* how you want to respond to the request, nice for wrapping the output in more html, for example */
  def respond(title: String)(body: PageFormat): ResponseFunction[Any]

  override final lazy val intent = (editors.map(EditorUnfiltered(_).intent) :+ resourceIntent).reduce(_ orElse _)

  case class EditorUnfiltered[ID](editor: EditorAbstract[ID]) extends Extractors[ID] {
    val idCell    = editor.idCell
    val MountedAt = Seg.unapply(editor.mountedAt).get

    /* this exists to silence compiler about inferring Any */
    implicit class AnyResponseFunction(one: ResponseFunction[Any]){
      def ~~>(two: ResponseFunction[Any]) = one andThen two
    }

    def intent:Plan.Intent = {
      /* show table */
      case ContextPath(_, FuzzySeg(MountedAt)) ⇒
        respond(title = MountedAt.head)(editor.view)

      /* show table row*/
      case GET(ContextPath(_, FuzzySeg(MountedAt :+ Id(id)))) ⇒
        respond(title = s"${editor.tableName} for $id") (
          editor.viewRow(id)
        )

      /* delete row */
      case DELETE(ContextPath(_, FuzzySeg(MountedAt:+ Id(id)))) ⇒
        editor.delete(id) match {
          case Left(failed) ⇒
            BadRequest ~~> ResponseString(failed.toString)
          case Right(Deleted(table, _)) ⇒
            respond(s"deleted id $id from $table")(editor.view)
        }
      
      /* update row */
      case POST(ContextPath(_, FuzzySeg(MountedAt :+ Id(id)))) & ColUpdates(updates) ⇒
        updates.headOption match {
          case Some((columnName, value)) =>
            editor.update(id, columnName, value) match {
              case Left(failed) ⇒
                BadRequest ~~> ResponseString(failed.toString)
              case Right(u) ⇒
                Ok ~~> ResponseString(u.toString)
            }
          case _ => BadRequest ~~> ResponseString("No column -> value provided")
        }

      /* show create new row of table */
      case GET(ContextPath(_, FuzzySeg(MountedAt :+ "new"))) ⇒
        respond(title = s"new ${editor.tableName}") (
          editor.viewNew
        )

      /* create new row */
      case POST(ContextPath(_, FuzzySeg(MountedAt :+ "new"))) & ColUpdates(params) ⇒
        editor.create(params) match {
          case Left(errors)                    ⇒ BadRequest ~~> ResponseString(errors.ts.mkString("\n"))
          case Right(Created(table, Some(id))) ⇒ respond(s"created new $table")(editor.viewRow(id))
          case Right(Created(table, None))     ⇒ respond(s"created new $table")(editor.view)
        }
    }
  }
}
