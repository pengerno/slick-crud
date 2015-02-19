package no.penger
package crud

import com.typesafe.scalalogging.LazyLogging
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FunSuite

import scala.slick.driver.H2Driver

/**
* Here we wire up a test version of crud wired to use 'String' instead of 'NodeSeq'.
*  so it's a bit easier to test, and try all crucial crud operations on it
*/
class CrudTest
  extends FunSuite with TypeCheckedTripleEquals
  with CrudAbstract with testRenderers          /* crud with concretization */
  with StoreTables with StoreCrudInstances      /* test tables */
  with LazyLogging {

  override lazy val profile = H2Driver
  import profile.simple._

  override lazy val db = Database.forURL(
    url = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
    driver = "org.h2.Driver"
  )

  override type REQ = Unit
  override def userDetails(ignore: Unit) = "test"

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
    override def notifyUpdateFailure(req: Unit)(f: CrudFailure) = {
      f match {
        case UpdateFailed(_, _, _, _, ErrorExc(t)) => t.printStackTrace()
        case _ ⇒ ()
      }
      fail(f.toString)
    }
  }

  object failOnUpdateSucceed extends UpdateNotifier {
    override def notifyUpdated(req: Unit)(s: CrudSuccess) = super.notifyUpdated(req)(s)
  }

  /* some test data */
  val ignoreMounted = "mounted"
  val storeId       = StoreId("store")
  val ignore        = ProductId(Int.MaxValue)
  val n1            = Name("amazing product")
  val n2            = Name("nights")
  val n3            = Name("new name for product three")
  val q1            = 100

  def Ed[ID: ColumnType : Cell, TABLE <: AbstractTable[_], LP, P]
        (ref: TableRef[ID, TABLE, LP, P], n: UpdateNotifier = noop) =
    Editor(ref, n)

  val (pid1, pid2) = db.withSession{implicit s ⇒
    Stores.insert(Store(storeId, Name("store"), None, closed = true))
    val pid1 = insertProduct(Product(ignore, n1, q1, storeId))
    val pid2 = insertProduct(Product(ignore, n2, 100, storeId))
    (pid1, pid2)
  }

  test("view()"){
    val e = Ed(TableRef(ignoreMounted, Products)(_.id))

    /* check that view contains both */
    val view: PageFormat = e.view
    containAssert(shouldContain = true, view, n1.value)
    containAssert(shouldContain = true, view, n2.value)
  }

  test("view(id)"){
    val e = Ed(TableRef(ignoreMounted, Products)(_.id))

    /* check that asking for one id only returns values for that product*/
    containAssert(shouldContain = true,  e.viewRow(pid1), n1.value)
    containAssert(shouldContain = false, e.viewRow(pid1), n2.value)

    containAssert(shouldContain = false, e.viewRow(pid2), n1.value)
    containAssert(shouldContain = true,  e.viewRow(pid2), n2.value)

    containAssert(shouldContain = false, e.viewRow(ProductId(-1)), n1.value)
    containAssert(shouldContain = false, e.viewRow(ProductId(-1)), n2.value)
  }

  test("view(id) for projection"){
    val e = Ed(TableRef(ignoreMounted, Products)(_.id).projected(_.map(p ⇒ (p.quantity, p.name))))

    val expected  = Left((Some(pid1.id.toString), Some(Seq(q1.toString, n1.value))))
    assert(e.viewRow(pid1).head.content === expected)
  }

  test("work for projection that doesnt include id row"){
    val e = Ed(TableRef(ignoreMounted, Products)(_.id).projected(_.map(p ⇒ (p.quantity, p.name))))
    assert(e.view.size > 0)
  }

  test("update (class ⇒ tuple) editor"){
    val e   = Ed(TableRef(ignoreMounted, Products)(_.id).projected(_.map(r ⇒ (r.id, r.name))), failOnUpdateFail)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, 100, storeId)))

    e.update((), pid, ColumnName("name"), n3.value)

    val expected = Left((Some(pid.id.toString), Some(Seq(pid.id.toString, n3.value))))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update (tuple ⇒ tuple) editor"){

    val e   = Ed(TableRef(ignoreMounted, ProductsTupled)(_.id).projected(_.map(r ⇒ (r.quantity, r.name))), failOnUpdateFail)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update((), pid, ColumnName("name"), n3.value)

    val expected = Left((Some(pid.id.toString), Some(Seq(q1.toString, n3.value))))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update (tuple ⇒ sorted tuple) editor"){

    val e   = Ed(TableRef(ignoreMounted, ProductsTupled)(_.id).projected(_.sortBy(_.quantity).map(r ⇒ (r.quantity, r.name))), failOnUpdateFail)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update((), pid, ColumnName("name"), n3.value)

    val expected = Left((Some(pid.id.toString), Some(Seq(q1.toString, n3.value))))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update (class ⇒ class) editor"){
    val e   = Ed(TableRef(ignoreMounted, Products)(_.id), failOnUpdateFail)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update((), pid, ColumnName("name"), n3.value)

    val expected = Left((Some(pid.id.toString), Some(Seq(pid.id.toString, n3.value, q1.toString, storeId.value))))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("update only chosen columns"){
    val e        = Ed(TableRef(ignoreMounted, Products)(_.id).projected(_.map(r ⇒ (r.id, r.soldBy))), failOnUpdateSucceed)
    val pid      = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, 100, storeId)))
    val colName  = ColumnName("name")
    val res      = e.update((), pid, colName, n3.value)
    val expected = Left(UpdateFailed(e.tableName, colName, cellProductId.toStr(pid), n3.value, ErrorMsg("projection has no cell with name name")))
    assert(res === expected)
  }

  test("update only valid id") {
    val e   = Ed(TableRef(ignoreMounted, Products)(_.id), failOnUpdateSucceed)

    db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    val colName         = ColumnName("name")
    val nonExistingPid  = ProductId(10001)
    val res             = e.update((), nonExistingPid, colName, n3.value)

    assert(true === res.isLeft)
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

    val e       = Ed(TableRef(ignoreMounted, Stores)(_.id), failOnUpdateFail)
    val sid     = StoreId("asdasdsad")
    val colName = ColumnName("description")

    db.withSession{implicit s ⇒ Stores.insert((sid, Name("fin butikk"), None, false))}

    val res1 = e.update((), sid, colName, "")
    assert(res1 === Right(Updated(e.tableName, colName, cellStoreId.toStr(sid), Some(""), "")))

    val res2 = e.update((), sid, colName, "arne")
    assert(res2 === Right(Updated(e.tableName, colName, cellStoreId.toStr(sid), Some(""), "arne")))
  }

  test("updating value that didnt exist") {
    class StoreT(tag: Tag) extends Table[(StoreId, Name, Option[Desc], Boolean)](tag, "stores") {
      def id       = column[StoreId]("id")
      def name     = column[Name]   ("name")
      def descr    = column[Desc]   ("description").?
      val closed   = column[Boolean]("closed")
      def *        = (id, name, descr, closed)
    }
    val Stores = TableQuery[StoreT]

    val e       = Ed(TableRef(ignoreMounted, Stores)(_.id), failOnUpdateFail)
    val sid     = StoreId("asdasdsad2")
    val colName = ColumnName("description")

    db.withSession{implicit s ⇒ Stores.insert((sid, Name("fin butikk"), None, false))}

    val ret = e.update((), sid, colName, "arne")
    assert(ret === Right(Updated(e.tableName, colName, cellStoreId.toStr(sid), Some(""), "arne")))
  }

  test("update when id column not selected"){
    val e   = Ed(TableRef(ignoreMounted, Products)(_.id).projected(_.map(_.name)), failOnUpdateFail)
    val pid = db.withSession(implicit s ⇒ insertProduct(Product(ignore, n2, q1, storeId)))

    e.update((), pid, ColumnName("name"), n3.value)

    val expected = Left((Some(pid.id.toString), Some(Vector(n3.value))))
    assert(e.viewRow(pid).head.content === expected)
  }

  test("create tupled"){
    val e   = Ed(TableRef(ignoreMounted, ProductsTupled)(_.id), noop)
    val ret = e.create((),
      Map[ColumnName, String](
        ColumnName("id")        → ignore.id.toString,
        ColumnName("name")      → n1.value,
        ColumnName("quantity")  → q1.toString,
        ColumnName("sold_by")   → storeId.value.toString
      )
    )
    assert(ret.isRight)
  }

  test("create class"){
    val quantity = 256
    val e   = Ed(TableRef(ignoreMounted, Products)(_.id).projected(_.sortBy(_.name)))
    val ret = e.create((),
      Map[ColumnName, String](
        ColumnName("id")        → ignore.id.toString,
        ColumnName("name")      → n1.value,
        ColumnName("quantity")  → quantity.toString,
        ColumnName("sold_by")   → storeId.value.toString
      )
    )
    /* test that view returns correctly after successful create*/
    ret match {
      case Left(CreateFailed(_, fs))    ⇒ fail(fs.head)
      case Right(Created(_, None))      ⇒ fail("no id found")
      case Right(Created(_, Some(pid))) ⇒
        val view = e.viewRow(cellProductId.fromStr(pid).right.get)
        containAssert(shouldContain = true, view, quantity.toString)
    }
  }

  test("create only with all columns specified"){
    val e   = Ed(TableRef(ignoreMounted, ProductsTupled)(_.id).projected(_.map(r ⇒ (r.name, r.quantity, r.soldByRef))))
    val ret = e.create((),
      Map[ColumnName, String](
        ColumnName("id")        → ignore.id.toString,
        ColumnName("quantity")  → q1.toString,
        ColumnName("sold_by")   → storeId.value.toString
      )
    )
    assert(Left(ErrorMsg("Didn't provide value for name")) === ret.left.map(_.ts.head))
  }

  test("create without auto-increment"){
    val e   = Ed(TableRef(ignoreMounted, Stores)(_.id))
    val sid = "storeId"
    val ret = e.create((),
      Map[ColumnName, String](
        ColumnName("id")          → sid,
        ColumnName("name")        → "my store",
        ColumnName("description") → storeId.value.toString,
        ColumnName("closed")      → true.toString
      )
    )
    assert(Right(Created(e.tableName, Some(sid))) === ret)
  }

  test("delete"){
    val e    = Ed(TableRef(ignoreMounted, Products)(_.id))
    val pid1 = db.withTransaction(implicit s ⇒ insertProduct(Product(ignore, n1, q1, storeId)))

    assert(e.viewRow(pid1).head.content === Left((Some(cellProductId.toStr(pid1)), Some(Seq(pid1.id.toString, n1.value, q1.toString, storeId.value)))))
    assert(Right(Deleted(e.tableName, cellProductId.toStr(pid1))) === e.delete((), pid1))
    assert(e.viewRow(pid1).isEmpty)
  }
}