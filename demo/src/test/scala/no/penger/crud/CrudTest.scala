package no.penger.crud

import com.typesafe.scalalogging.LazyLogging
import no.penger.db.LiquibaseH2TransactionComponent
import org.scalatest.FunSuite
import org.scalactic.TypeCheckedTripleEquals

/**
 * Here we wire up a test version of crud wired to use 'String' instead of 'NodeSeq'.
 *  so it's a bit easier to test, and try all crucial crud operations on it
 */
class CrudTest
  extends FunSuite with TypeCheckedTripleEquals
  with CrudAbstract with testView                       /* crud with concretization */
  with StoreTables with StoreCrudInstances              /* test tables */
  with LiquibaseH2TransactionComponent with LazyLogging /* h2 with tables */ {

  import profile.simple._

  /* some tests use this table in this non-projected variant*/
  class ProductTupledT(tag: Tag) extends Table[(ProductId, Name, Int, StoreId)](tag, "products") {
    def id        = column[ProductId]("id", O.PrimaryKey, O.AutoInc)
    def name      = column[Name]     ("name")
    def quantity  = column[Int]      ("quantity")
    def soldByRef = column[StoreId]  ("sold_by")
    def *        = (id, name, quantity, soldByRef)
  }
  val ProductsTupled = TableQuery[ProductTupledT]

  /* some utility functions */
  def insertProduct(p: Product)(implicit s: Session): ProductId =
    (Products returning Products.map(_.id)).insert(p)

  def containAssert(shouldContain: Boolean, haystack: PageFormat, needle: String) = {
    assert(shouldContain == (haystack./*YOLO*/toString() contains needle), haystack)
  }

  def fail(e: Error): Nothing = e match {
    case ErrorExc(t)   ⇒ fail(t)
    case ErrorMsg(msg) ⇒ fail(msg)
  }

  val noop = new UpdateNotifier
  object failOnUpdateFail extends UpdateNotifier {
    override def notifyUpdateFailure(f: res.Failure) = fail(f.toString)
  }

  object failOnUpdateSucceed extends UpdateNotifier {
    override def notifyUpdated(s: res.Success) = super.notifyUpdated(s)
  }

  /* some test data */
  val ignoreMounted = "mounted"
  val storeId       = StoreId("store")
  val ignore        = ProductId(Int.MaxValue)
  val n1            = Name("amazing product")
  val n2            = Name("nights")
  val n3            = Name("new name for product three")
  val q1            = 100

  val (pid1, pid2) = db.withSession{implicit s ⇒
    Stores.insert(Store(storeId, Name("store"), None, closed = true))
    val pid1 = insertProduct(Product(ignore, n1, q1, storeId))
    val pid2 = insertProduct(Product(ignore, n2, 100, storeId))
    (pid1, pid2)
  }

  test("view()"){
    val e = Editor(ignoreMounted, Products, noop)(identity, _.id)

    /* check that view contains both */
    val view: PageFormat = e.view
    containAssert(shouldContain = true, view, n1.asString)
    containAssert(shouldContain = true, view, n2.asString)
  }

  test("view(id)"){
    val e = Editor(ignoreMounted, Products, noop)(identity, _.id)

    /* check that asking for one id only returns values for that product*/
    containAssert(shouldContain = true,  e.viewRow(pid1), n1.asString)
    containAssert(shouldContain = false, e.viewRow(pid1), n2.asString)

    containAssert(shouldContain = false, e.viewRow(pid2), n1.asString)
    containAssert(shouldContain = true,  e.viewRow(pid2), n2.asString)

    containAssert(shouldContain = false, e.viewRow(ProductId(-1)), n1.asString)
    containAssert(shouldContain = false, e.viewRow(ProductId(-1)), n2.asString)
  }

  test("view(id) for projection"){
    val e = Editor(ignoreMounted, Products, noop)(_.map(p ⇒ (p.quantity, p.name)), _.id)

    val expected  = Left((Some(pid1.id.toString), Some(Seq(q1.toString, n1.asString))))
    assert(e.viewRow(pid1).head.content === expected)
  }

  test("update (class ⇒ tuple) editor"){
    val e   = Editor(ignoreMounted, Products, failOnUpdateFail)(_.map(r ⇒ (r.id, r.name)), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, 100, storeId)))

    e.update(pid, ColumnName("name"), n3.asString)

    val expected = Left(Some(pid.id.toString), Some(Seq(pid.id.toString, n3.asString)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update (tuple ⇒ tuple) editor"){

    val e   = Editor(ignoreMounted, ProductsTupled, failOnUpdateFail)(_.map(r ⇒ (r.quantity, r.name)), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, ColumnName("name"), n3.asString)

    val expected = Left(Some(pid.id.toString), Some(Seq(q1.toString, n3.asString)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update (tuple ⇒ sorted tuple) editor"){

    val e   = Editor(ignoreMounted, ProductsTupled, failOnUpdateFail)(_.sortBy(_.quantity).map(r ⇒ (r.quantity, r.name)), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, ColumnName("name"), n3.asString)

    val expected = Left(Some(pid.id.toString), Some(Seq(q1.toString, n3.asString)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update (class ⇒ class) editor"){
    val e   = Editor(ignoreMounted, Products, failOnUpdateFail)(identity, _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, ColumnName("name"), n3.asString)

    val expected = Left(Some(pid.id.toString), Some(Seq(pid.id.toString, n3.asString, q1.toString, storeId.id)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update only chosen columns"){
    val e   = Editor(ignoreMounted, Products, failOnUpdateSucceed)(_.map(r ⇒ (r.id, r.soldBy)), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, 100, storeId)))
    e.update(pid, ColumnName("name"), n3.asString)
  }

  test("update only valid id") {
    val e   = Editor(ignoreMounted, Products, failOnUpdateSucceed)(identity, _.id)
    db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))
    e.update(ProductId(10001), ColumnName("name"), n3.asString)
  }

  test("option-mapping in table projection") {
    class StoreT(tag: Tag) extends Table[(StoreId, Name, Option[Desc], Boolean)](tag, "stores") {
      def id       = column[StoreId]("id")
      def name     = column[Name]   ("name")
      def descr    = column[Desc]   ("description")
      val closed   = column[Boolean]("closed")
      def *        = (id, name, descr.?, closed) //<-- map desc to Option[Desc] in projection
    }
    val Stores = TableQuery[StoreT]

    val e   = Editor(ignoreMounted, Stores, failOnUpdateFail)(identity, _.id)
    val sid = StoreId("asdasdsad")

    db.withSession{implicit s ⇒ Stores.insert((sid, Name("fin butikk"), Some(Desc("π")), false))}
    e.update(sid, ColumnName("description"), "")
    e.update(sid, ColumnName("description"), "arne")
  }

  test("update when id column not selected"){
    val e   = Editor(ignoreMounted, Products, failOnUpdateFail)(_.map(_.name), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, ColumnName("name"), n3.asString)

    val expected = Left(Some(pid.id.toString), Some(Vector(n3.asString)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("create tupled"){
    val e   = Editor(ignoreMounted, ProductsTupled, noop)(identity, _.id)
    val ret = e.create(
      Map[ColumnName, String](
        ColumnName("id")        → ignore.id.toString,
        ColumnName("name")      → n1.asString,
        ColumnName("quantity")  → q1.toString,
        ColumnName("sold_by")   → storeId.id.toString
      )
    )
    assert(ret.isRight)
  }

  test("create class"){
    val quantity = 256
    val e   = Editor(ignoreMounted, Products, noop)(_.sortBy(_.name), _.id)
    val ret = e.create(
      Map[ColumnName, String](
        ColumnName("id")        → ignore.id.toString,
        ColumnName("name")      → n1.asString,
        ColumnName("quantity")  → quantity.toString,
        ColumnName("sold_by")   → storeId.id.toString
      )
    )
    /* test that view returns correctly after successful create*/
    ret match {
      case Left(res.CreateFailed(_, fs)) ⇒ fail(fs.head)
      case Right(res.Created(_, pid)) ⇒
        val view = e.viewRow(pid)
        containAssert(shouldContain = true, view, quantity.toString)
    }
  }

  test("create only with all columns specified"){
    val e   = Editor(ignoreMounted, ProductsTupled, noop)(_.map(r ⇒ (r.name, r.quantity, r.soldByRef)), _.id)
    val ret = e.create(
      Map[ColumnName, String](
        ColumnName("id")        → ignore.id.toString,
        ColumnName("quantity")  → q1.toString,
        ColumnName("sold_by")   → storeId.id.toString
      )
    )
    assert(Left(ErrorMsg("Didn't provide value for name")) === ret.left.map(_.ts.head))
  }

  test("create without auto-increment"){
    val e   = Editor(ignoreMounted, Stores, noop)(identity, _.id)
    val sid = StoreId("storeId")
    val ret = e.create(
      Map[ColumnName, String](
        ColumnName("id")          → sid.id,
        ColumnName("name")        → "my store",
        ColumnName("description") → storeId.id.toString,
        ColumnName("closed")      → true.toString
      )
    )
    assert(Right(res.Created(e.tableName, sid)) === ret)
  }

  test("delete"){
    val e    = Editor(ignoreMounted, Products, noop)(identity, _.id)
    val pid1 = db.withTransaction(implicit s ⇒ insertProduct(Product(ignore, n1, q1, storeId)))

    assert(e.viewRow(pid1).head.content === Left((Some(Cell.toStr(pid1)), Some(Seq(pid1.id.toString, n1.asString, q1.toString, storeId.id)))))
    assert(Right(res.Deleted(e.tableName, pid1)) === e.delete(pid1))
    assert(e.viewRow(pid1).head.content === Left((Some(Cell.toStr(pid1)), None)))
  }
}