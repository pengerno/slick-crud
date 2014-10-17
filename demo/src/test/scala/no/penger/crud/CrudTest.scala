
package no.penger.crud

import com.typesafe.scalalogging.slf4j.LazyLogging
import no.penger.db.LiquibaseH2TransactionComponent
import org.scalatest.FunSuite
import org.scalautils.TypeCheckedTripleEquals

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

  object failOnUpdateFail extends UpdateNotifier {
    override def updateFailed[ID](t: TableName, id: ID)(f: FailedUpdate) = {
      f.t.printStackTrace()
      fail(f.t)
    }
  }
  object failOnUpdateSucceed extends UpdateNotifier {
    override def updated[ID, T](t: TableName, id: ID)(u: Update) = fail(s"should not have been able to update: $u")
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
    val e = Editor(ignoreMounted, Products)(identity, _.id)

    /* check that view contains both */
    val view: PageFormat = e.view
    containAssert(shouldContain = true, view, n1.asString)
    containAssert(shouldContain = true, view, n2.asString)
  }

  test("view(id)"){
    val e = Editor(ignoreMounted, Products)(identity, _.id)

    /* check that asking for one id only returns values for that product*/
    containAssert(shouldContain = true,  e.viewRow(pid1), n1.asString)
    containAssert(shouldContain = false, e.viewRow(pid1), n2.asString)

    containAssert(shouldContain = false, e.viewRow(pid2), n1.asString)
    containAssert(shouldContain = true,  e.viewRow(pid2), n2.asString)

    containAssert(shouldContain = false, e.viewRow(ProductId(-1)), n1.asString)
    containAssert(shouldContain = false, e.viewRow(ProductId(-1)), n2.asString)
  }

  test("view(id) for projection"){
    val e = Editor(ignoreMounted, Products)(_.map(p ⇒ (p.quantity, p.name)), _.id)

    val expected  = Left((Some(pid1.id.toString), Some(Seq(q1.toString, n1.asString))))
    assert(e.viewRow(pid1).head.content === expected)
  }

  test("update (class ⇒ tuple) editor"){
    val e   = Editor(ignoreMounted, Products, failOnUpdateFail)(_.map(r ⇒ (r.id, r.name)), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, 100, storeId)))

    e.update(pid, Map(ColumnName("name") → n3.asString))

    val expected = Left(Some(pid.id.toString), Some(Seq(pid.id.toString, n3.asString)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update (tuple ⇒ tuple) editor"){

    val e   = Editor(ignoreMounted, ProductsTupled, failOnUpdateFail)(_.map(r ⇒ (r.quantity, r.name)), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, Map(ColumnName("name") → n3.asString))

    val expected = Left(Some(pid.id.toString), Some(Seq(q1.toString, n3.asString)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update (tuple ⇒ sorted tuple) editor"){

    val e   = Editor(ignoreMounted, ProductsTupled, failOnUpdateFail)(_.sortBy(_.quantity).map(r ⇒ (r.quantity, r.name)), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, Map(ColumnName("name") → n3.asString))

    val expected = Left(Some(pid.id.toString), Some(Seq(q1.toString, n3.asString)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update (class ⇒ class) editor"){
    val e   = Editor(ignoreMounted, Products, failOnUpdateFail)(identity, _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, Map(ColumnName("name") → n3.asString))

    val expected = Left(Some(pid.id.toString), Some(Seq(pid.id.toString, n3.asString, q1.toString, storeId.id)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update only chosen columns"){
    val e   = Editor(ignoreMounted, Products, failOnUpdateSucceed)(_.map(r ⇒ (r.id, r.soldByRef)), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, 100, storeId)))
    e.update(pid, Map(ColumnName("name") → n3.asString))
  }

  test("update only valid id") {
    val e   = Editor(ignoreMounted, Products, failOnUpdateSucceed)(identity, _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))
    e.update(ProductId(10001), Map(ColumnName("name") → n3.asString))
  }

  test("update when id column not selected"){
    val e   = Editor(ignoreMounted, Products, failOnUpdateFail)(_.map(_.name), _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, Map(ColumnName("name") → n3.asString))

    val expected = Left(Some(pid.id.toString), Some(Vector(n3.asString)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update two columns"){
    val e   = Editor(ignoreMounted, Products, failOnUpdateFail)(identity, _.id)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))
    val newQuantity = 101
    e.update(pid,
      Map(
        ColumnName("name")     → n3.asString,
        ColumnName("quantity") → newQuantity.toString)
    )

    val expected = Left(Some(pid.id.toString), Some(Seq(pid.id.toString, n3.asString, newQuantity.toString, storeId.id)))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("create tupled"){
    val e   = Editor(ignoreMounted, ProductsTupled)(identity, _.id)
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
    val e   = Editor(ignoreMounted, Products)(_.sortBy(_.name), _.id)
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
      case Left(fs)         ⇒ fail("couldn't update", fs.head)
      case Right(pid) ⇒
        val view = e.viewRow(pid)
        containAssert(shouldContain = true, view, quantity.toString)
    }
  }

  test("create only with all columns specified"){
    val e   = Editor(ignoreMounted, ProductsTupled)(_.map(r ⇒ (r.name, r.quantity, r.soldByRef)), _.id)
    val ret = e.create(
      Map[ColumnName, String](
        ColumnName("id")        → ignore.id.toString,
        ColumnName("quantity")  → q1.toString,
        ColumnName("sold_by")   → storeId.id.toString
      )
    )
    assert(Left("didn't provide value for name") === ret.left.map(_.head.getMessage))
  }

  test("create without auto-increment"){
    val e   = Editor(ignoreMounted, Stores)(identity, _.id)
    val sid = StoreId("storeId")
    val ret = e.create(
      Map[ColumnName, String](
        ColumnName("id")          → sid.id,
        ColumnName("name")        → "my store",
        ColumnName("description") → storeId.id.toString,
        ColumnName("closed")      → true.toString
      )
    )
    assert(Right(sid) === ret)
  }
}