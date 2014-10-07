package no.penger.crud

import com.typesafe.scalalogging.slf4j.LazyLogging
import no.penger.db.LiquibaseH2TransactionComponent
import org.scalatest.FunSuite
import org.scalautils.TypeCheckedTripleEquals

import scala.util.Try

/**
 * Here we wire up a test version of crud wired to use 'String' instead of 'NodeSeq'.
 *  so it's a bit easier to test, and try all crucial crud operations on it
 */
class CrudTest
  extends FunSuite with TypeCheckedTripleEquals
  with CrudAbstract with testView                       /* crud with concretization */
  with StoreTables                                      /* test tables */
  with LiquibaseH2TransactionComponent with LazyLogging /* h2 with tables */
{

  def stringCell[E](toStr: E ⇒ String, fromStr: String ⇒ E) = new Cell[E] {
    override def link(ctx: String, e: E) = toStr(e)
    override def editable(e: E) = toStr(e)
    override def tryCast(value: String) = Try(fromStr(value))
    override def fixed(e: E) = toStr(e)
  }

  implicit val t0 = stringCell[Desc](_.asString, Desc)
  implicit val t1 = stringCell[Name](_.asString, Name)
  implicit val t2 = stringCell[ProductId](_.id.toString, s => ProductId(s.toLong))
  implicit val t3 = stringCell[StoreId](_.id, StoreId)
  implicit val t4 = stringCell[Int](_.toString, _.toInt)

                                  /* my apologies, this has nothing to do with scala.Product */
  implicit val e0 = mappedCellRow(Product.unapply)

  import profile.simple._

  /* some utility functions */
  def insertProduct(p: Product)(implicit s: Session): ProductId = (Products returning Products.map(_.id)).insert(p)

  def containAssert(shouldContain: Boolean, haystack: PageFormat, needle: String) = {
    assert(shouldContain == (haystack./*YOLO*/toString() contains needle), haystack)
  }

  object failOnUpdateFail extends UpdateNotifier {
    override def updateFailed[ID](t: TableName, id: ID)(f: FailedUpdate): Unit = fail("should be able to update: " + f.toString)
  }
  object failOnUpdateSucceed extends UpdateNotifier {
    override def updated[ID, T](t: TableName, id: ID)(u: Update): Unit = fail(s"should not have been able to update: $u")
  }

  /* some test data */
  val storeId = StoreId("store")
  val ignore  = ProductId(Int.MaxValue)
  val n1      = Name("amazing product")
  val n2      = Name("nights")
  val n3      = Name("new name for product three")
  val q1      = 100

  val (pid1, pid2) = db.withSession{implicit s =>
    Stores.insert(Store(storeId, Name("store"), None))
    val pid1 = insertProduct(Product(ignore, n1, q1, storeId))
    val pid2 = insertProduct(Product(ignore, n2, 100, storeId))
    (pid1, pid2)
  }

  test("view()"){
    val e = Editor("mounted", Products)(identity, _.id)

    /* check that view contains both */
    containAssert(shouldContain = true, e.view("ctx"), n1.asString)
    containAssert(shouldContain = true, e.view("ctx"), n2.asString)
  }

  test("view(id)"){
    val e = Editor("mounted", Products)(identity, _.id)

    /* check that asking for one id only returns values for that product*/
    containAssert(shouldContain = true,  e.viewRow("ctx", pid1), n1.asString)
    containAssert(shouldContain = false, e.viewRow("ctx", pid1), n2.asString)

    containAssert(shouldContain = false, e.viewRow("ctx", pid2), n1.asString)
    containAssert(shouldContain = true,  e.viewRow("ctx", pid2), n2.asString)

    containAssert(shouldContain = false, e.viewRow("ctx", ProductId(-1)), n1.asString)
    containAssert(shouldContain = false, e.viewRow("ctx", ProductId(-1)), n2.asString)
  }

  test("view(id) for projection"){
    val e = Editor("mounted", Products)(_.map(p => (p.quantity, p.name)), _.id)

    val tableName = TableName("products")
    val expected = Seq(
      TestView(
        tableName,
        Seq(tableName.withColumn(ColumnName("quantity")), tableName.withColumn(ColumnName("name"))),
        Left((Some(pid1.id.toString), Some(Seq(q1.toString, n1.asString))))
      )
    )

    assert(e.viewRow("ctx", pid1) === expected)
  }

  test("update (class => tuple) editor"){
    val e   = Editor("mounted", Products, failOnUpdateFail)(_.map(r => (r.id, r.name)), _.id)
    val pid = db.withSession(implicit s => insertProduct(Product(ignore, n2, 100, storeId)))

    e.update(pid, Map(ColumnName("name") → n3.asString))

    val expected = Left(Some(pid.id.toString), Some(Seq(pid.id.toString, n3.asString)))
    assert(e.viewRow("ctx", pid).head.content === expected)
  }

  test("update (tuple => tuple) editor"){

    class ProductT2(tag: Tag) extends Table[(Name, Int)](tag, "products") {
      def id        = column[ProductId]("id", O.PrimaryKey, O.AutoInc)
      def name      = column[Name]     ("name")
      def quantity  = column[Int]      ("quantity")
      def *        = (name, quantity)
    }
    val Products2 = TableQuery[ProductT2]

    val e   = Editor("mounted", Products2, failOnUpdateFail)(_.map(r => (r.quantity, r.name)), _.id)
    val pid = db.withSession(implicit s => insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, Map(ColumnName("name") → n3.asString))

    val expected = Left(Some(pid.id.toString), Some(Seq(q1.toString, n3.asString)))
    assert(e.viewRow("ctx", pid).head.content === expected)
  }

  test("update (class => class) editor"){
    val e   = Editor("mounted", Products, failOnUpdateFail)(identity, _.id)
    val pid = db.withSession(implicit s => insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, Map(ColumnName("name") → n3.asString))

    val expected = Left(Some(pid.id.toString), Some(Seq(pid.id.toString, n3.asString, q1.toString, storeId.id)))
    assert(e.viewRow("ctx", pid).head.content === expected)
  }

  test("only update chosen columns"){
    val e   = Editor("mounted", Products, failOnUpdateSucceed)(_.map(r => (r.id, r.soldByRef)), _.id)
    val pid = db.withSession(implicit s => insertProduct(Product(ignore, n2, 100, storeId)))
    e.update(pid, Map(ColumnName("name") → n3.asString))
  }

  test("update when id column not selected"){
    val e   = Editor("mounted", Products, failOnUpdateFail)(_.map(_.name), _.id)
    val pid = db.withSession(implicit s => insertProduct(Product(ignore, n2, q1, storeId)))

    e.update(pid, Map(ColumnName("name") → n3.asString))

    val expected = Left(Some(pid.id.toString), Some(Vector(n3.asString)))
    assert(e.viewRow("ctx", pid).head.content === expected)
  }

  test("update two columns"){
    val e   = Editor("mounted", Products, failOnUpdateFail)(identity, _.id)
    val pid = db.withSession(implicit s => insertProduct(Product(ignore, n2, q1, storeId)))
    val newQuantity = 101
    e.update(pid,
      Map(
        ColumnName("name")     → n3.asString,
        ColumnName("quantity") → newQuantity.toString)
    )

    val expected = Left(Some(pid.id.toString), Some(Seq(pid.id.toString, n3.asString, newQuantity.toString, storeId.id)))
    assert(e.viewRow("ctx", pid).head.content === expected)
  }

}
