package no.penger.crud
package http

import unfiltered.filter.Plan
import unfiltered.filter.Plan.Intent
import unfiltered.filter.request.ContextPath
import unfiltered.request.{&, GET, POST, Params, Seg}
import unfiltered.response._

trait unfilteredIntegration extends editorAbstracts {

  def respond(ctx: String, title: String)(body: ViewFormat): ResponseFunction[Any]

  class EditorUnfiltered[ID](mount: String, editor: EditorAbstract[ID]){

    val MountedAt = Seg.unapply(mount).get
    def base(ctx: String) = (ctx :: MountedAt).mkString("/")

    /* extract id from url */
    object Id {
      def unapply(parts:List[String]) =
        parts.splitAt(MountedAt.size) match {
          case (MountedAt, id :: Nil) => editor.idCell.tryCast(id).toOption
          case _ => None
        }
    }

    def intent:Plan.Intent = {

      case req@GET(ContextPath(ctx, Seg(MountedAt))) =>
        respond(ctx, title = MountedAt.head)(editor.view(base(ctx)))

      case req@GET(ContextPath(ctx, Seg(Id(id)))) =>
        respond(ctx, title = s"${editor.tableName} for $id") (
          editor.viewRow(base(ctx), id)
        )

      case req@POST(ContextPath(_, Seg(Id(id)))) & Params(params) =>
        val updates = params.map {
          case (name, values) => ColumnName(name) -> values.head
        }
        editor.update(id, updates) match {
          case Left(fails: Seq[FailedUpdate]) =>
            BadRequest ~> ResponseString(fails.mkString("\n"))
          case Right(okUpdates) =>
            Ok ~> ResponseString(okUpdates.mkString("\n"))
        }
    }
  }


  trait CrudPlan extends Plan {
    def editors: Map[String, EditorAbstract[_]]
    def resourceIntent: Intent
    lazy val editorIntents = editors.map{case (mount, r) => new EditorUnfiltered(mount, r)}.map(_.intent).toSeq
    override lazy val intent = (editorIntents :+ resourceIntent).reduce(_ orElse _)
  }
}
