package no.penger.crud

import javax.servlet.http.HttpServletRequest

import unfiltered.filter.Plan
import unfiltered.request._
import unfiltered.response._
import unfiltered.util.Of.Int

trait unfilteredIntegration extends Plan with editorAbstracts with extractors with resources with urls {

  override final type REQ = HttpRequest[HttpServletRequest]

  /* all the editors you want to expose */
  def editors: Seq[EditorAbstract[_]]

  /* how you want to respond to the request, nice for wrapping the output in more html, for example */
  def respond(title: String)(body: PageFormat): ResponseFunction[Any]

  override final lazy val intent = (editors.map(EditorUnfiltered(_).intent) :+ resourceIntent).reduce(_ orElse _)

  case class EditorUnfiltered[ID](editor: EditorAbstract[ID]) extends Extractors[ID] with EditorUrls {

    override val mountedAt = editor.mountedAt
    override val idCell    = editor.idCell

    /* this exists to silence compiler about inferring Any */
    implicit class AnyResponseFunction(one: ResponseFunction[Any]){
      def ~~>(two: ResponseFunction[Any]) = one andThen two
    }
    
    def respondMessage(res: ResponseFunction[Any], msg: String) =
      res ~~> respond(msg)(editor.message(msg))

    def intent:Plan.Intent = {
      case Path(url.Table()) ⇒
        Redirect(url.ReadPage("0"))

      case Path(url.Read()) ⇒
        Redirect(url.ReadPage("0"))

      case Path(url.ReadPage(Int(page))) ⇒
        respond(title = editor.tableName.toString)(editor.view(page))

      /* show table row*/
      case Path(url.ReadRow(Id(id))) ⇒
        respond(title = s"${editor.tableName} for $id") (
          editor.viewRow(id)
        )

      /* show form to create new row*/
      case GET(Path(url.CreateRow())) ⇒
        respond(title = s"new ${editor.tableName}") (
          editor.viewNew
        )

      /* create new row */
      case req@POST(Path(url.CreateRow())) & ColUpdates(params) ⇒
        editor.create(req, params) match {
          case Left(errors)                           ⇒ respondMessage(BadRequest, errors.ts.mkString("\n"))
          case Right(Created(_, table, Some(Id(id)))) ⇒ respond(s"created new $table")(editor.viewRow(id))
          case Right(Created(_, table, _))            ⇒ respond(s"created new $table")(editor.view(0))
        }


      /* delete row */
      case req@DELETE(Path(url.DeleteRow(Id(id)))) ⇒
        editor.delete(req, id) match {
          case Left(failed) ⇒
            respondMessage(BadRequest, failed.toString)
          case Right(Deleted(_, table, _)) ⇒
            respondMessage(Ok, s"Deleted id $id from $table")
        }

      /* update row */
      case req@POST(Path(url.UpdateRow(Id(id)))) & ColUpdates(updates) ⇒
        updates.headOption match {
          case Some((columnName, value)) =>
            editor.update(req, id, columnName, value) match {
              case Left(failed) ⇒
                BadRequest ~~> ResponseString(failed.toString)
              case Right(u) ⇒
                Ok ~~> ResponseString(u.toString)
            }
          case _ => BadRequest ~~> ResponseString("No column -> value provided")
        }
    }
  }
}
