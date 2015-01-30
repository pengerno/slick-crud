package no.penger
package crud

import javax.servlet.ServletContext

import com.typesafe.scalalogging.LazyLogging
import unfiltered.filter.Plan
import unfiltered.response._

import scala.slick.driver.H2Driver
import scala.xml.NodeSeq

trait StoreDomain{
  case class Name(value: String)
  case class Desc(value: String)

  case class StoreId(value: String)
  case class Store(id: StoreId, name: Name, description: Option[Desc], closed: Boolean)

  case class ProductId(id: Long)
  case class Product(id: ProductId, name: Name, quantity: Int, soldBy: StoreId)

  case class EmployeeId(id: Long)
  case class Employee(id: EmployeeId, name: Name, worksAt: Option[StoreId], role: Option[Role], good: Option[Boolean])

  sealed abstract class Role(val name: String)

  object Role{
    case object Employee extends Role("Employee")
    case object Manager extends Role("Manager")

    val values = List(Employee, Manager)
    def get(s: String) = s match {
      case Employee.name ⇒ Employee
      case Manager.name  ⇒ Manager
    }
  }
}

trait StoreTables extends StoreDomain with slickIntegration {
  import profile.simple._

  /* these type class instances are to enable the use of the types in slick */
  implicit lazy val m1 = MappedColumnType.base[Desc,       String](_.value, Desc)
  implicit lazy val m2 = MappedColumnType.base[EmployeeId, Long]  (  _.id,  EmployeeId)
  implicit lazy val m3 = MappedColumnType.base[Name,       String](_.value, Name)
  implicit lazy val m4 = MappedColumnType.base[ProductId,  Long]  (  _.id,  ProductId)
  implicit lazy val m5 = MappedColumnType.base[StoreId,    String](_.value, StoreId)
  implicit lazy val m6 = MappedColumnType.base[Role,       String](_.toString, Role.get)

  class StoreT(tag: Tag) extends Table[Store](tag, "stores") {
    def id        = column[StoreId]("id")
    def name      = column[Name]   ("name")
    def descr     = column[Desc]   ("description", O.Nullable).?
    def closed    = column[Boolean]("closed")
    def *         = (id, name, descr, closed) <> (Store.tupled, Store.unapply)
  }
  val Stores = TableQuery[StoreT]

  class ProductT(tag: Tag) extends Table[Product](tag, "products") {
    def id        = column[ProductId]("id", O.PrimaryKey, O.AutoInc)
    def name      = column[Name]     ("name")
    def quantity  = column[Int]      ("quantity")
    def soldBy    = column[StoreId]  ("sold_by")
    def *         = (id, name, quantity, soldBy) <> (Product.tupled, Product.unapply)
  }
  val Products = TableQuery[ProductT]

  class EmployeeT(tag: Tag) extends Table[Employee](tag, "employees"){
    val id        = column[EmployeeId]("id", O.PrimaryKey, O.AutoInc)
    val name      = column[Name]      ("name")
    val worksAt   = column[StoreId]   ("works_at", O.Nullable).?
    val role      = column[Role]      ("role", O.Nullable).?
    val good      = column[Boolean]   ("good", O.Nullable).?
    def *         = (id, name, worksAt, role, good) <> (Employee.tupled, Employee.unapply)
  }
  val Employees  = TableQuery[EmployeeT]

  db.withTransaction{
    implicit tx ⇒
      Stores.ddl.create
      Products.ddl.create
      Employees.ddl.create
  }
}

trait StoreCrudInstances extends StoreDomain with cellRowInstances {
  /**
   * we need to provide cell instances for every type we expose through slick-crud,
   *  in order for it to know how to render and parse them, analogously to slick
   */
  implicit val c1 = SimpleCell[Name](_.value, Name(_).ensuring(_.value.nonEmpty))
  implicit val c2 = SimpleCell[Desc](_.value, Desc(_).ensuring(_.value.nonEmpty))
  implicit val c3 = SimpleCell[StoreId](_.value, StoreId(_).ensuring(_.value.nonEmpty), isEditable = true)
  implicit val c4 = SimpleCell[ProductId](_.id.toString, s ⇒ ProductId(s.toLong))
  implicit val c5 = SimpleCell[EmployeeId](_.id.toString, s ⇒ EmployeeId(s.toLong))
  implicit val c6 = ConstrainedCell[Role](SimpleCell[Role](_.name, Role.get), None)(Role.values)
  /**
   * These cellRow mapping instances are necessary in order to expose
   *  tables that have default projections to non-tuple structures.
   */
  implicit val e1 = mappedCellRow(Employee.tupled, Employee.unapply)
  implicit val e2 = mappedCellRow(Product.tupled,  Product.unapply)
  implicit val e3 = mappedCellRow(Store.tupled,    Store.unapply)
}


object CrudDemoWebApp extends Plan with LazyLogging {
  lazy val profile         = H2Driver
  import profile.simple._

  val db = Database.forURL(
    url = s"jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
    driver = "org.h2.Driver"
  )

  class CrudUnfilteredDemo(context: ServletContext) extends StoreTables
                                                    with CrudUnfiltered
                                                    with StoreCrudInstances
                                                    with GenDataModule
                                                    with updateNotifierLogging {
    override lazy val profile = CrudDemoWebApp.profile
    override lazy val db      = CrudDemoWebApp.db
    override      val ctx     = context.getContextPath

    /* generate some data to play with */
    db.withTransaction{implicit s ⇒
      Stores    insertAll (GenData.stores    :_*)
      Employees insertAll (GenData.employees :_*)
      Products  insertAll (GenData.products  :_*)
    }

    object notifier extends UpdateNotifierLogging with LazyLogging

    val storesRef    = TableRef("/stores",    Stores, isEditable = true)(_.id)

    val employeeRef  = TableRef("/employees", Employees, isEditable = true)(_.id)
      .projected(_.sortBy(_.name.asc))
      .linkedOn(_.worksAt, storesRef)(_.id)(_ === _)

    val productsRef  = TableRef("/products",  Products)(_.id)
      .projected(_.map(t ⇒ (t.id, t.soldBy, t.quantity, t.name)))
      .linkedOn(_._2, storesRef)(_.id)(_ === _)

    val storesRefRef = storesRef
      .projected(_.sortBy(_.name))
      .linkedOn(_.id, employeeRef)(_.worksAt)(_ === _)
      .linkedOn(_.id, productsRef)(_._2)(_ === _)

    override val editors = Seq(
      Editor(employeeRef,  notifier),
      Editor(productsRef,  notifier),
      Editor(storesRefRef, notifier))

    override def respond(title: String)(body: NodeSeq): ResponseFunction[Any] =
      Html5(PageTemplate.page(ctx, title)(body))
  }

  override lazy val intent = new CrudUnfilteredDemo(config.getServletContext).intent
}