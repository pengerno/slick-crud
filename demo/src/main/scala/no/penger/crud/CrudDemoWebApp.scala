package no.penger
package crud

import javax.servlet.{FilterConfig, ServletContext}

import com.typesafe.scalalogging.slf4j.LazyLogging
import unfiltered.filter.Plan
import unfiltered.response._

import scala.language.implicitConversions
import scala.xml.NodeSeq

trait StoreDomain{
  case class Name(asString: String)
  case class Desc(asString: String)

  case class StoreId(id: String)
  case class Store(id: StoreId, name: Name, description: Option[Desc], closed: Boolean)

  case class ProductId(id: Long)
  case class Product(id: ProductId, name: Name, quantity: Int, soldBy: StoreId)

  case class EmployeeId(id: Long)
  case class Employee(id: EmployeeId, name: Name, worksAt: StoreId)
}

trait StoreTables extends StoreDomain with databaseIntegration {
  import profile.simple._

  /* these type class instances are to enable the use of the types in slick */
  implicit lazy val m1 = MappedColumnType.base[Desc,       String](_.asString, Desc)
  implicit lazy val m2 = MappedColumnType.base[EmployeeId, Long](  _.id,       EmployeeId)
  implicit lazy val m3 = MappedColumnType.base[Name,       String](_.asString, Name)
  implicit lazy val m4 = MappedColumnType.base[ProductId,  Long](  _.id,       ProductId)
  implicit lazy val m5 = MappedColumnType.base[StoreId,    String](_.id,       StoreId)

  class StoreT(tag: Tag) extends Table[Store](tag, "stores") {
    def id       = column[StoreId]("id")
    def name     = column[Name]   ("name")
    def descr    = column[Desc]   ("description").?
    def closed   = column[Boolean]("closed")

    def *        = (id, name, descr, closed) <> (Store.tupled, Store.unapply)

    def inventory = foreignKey("store_inventory", id.?, Products)(_.soldByRef.?)
    def employees = foreignKey("store_employees", id, Employees)(_.worksAtRef)
  }
  val Stores = TableQuery[StoreT]

  class ProductT(tag: Tag) extends Table[Product](tag, "products") {
    def id        = column[ProductId]("id", O.PrimaryKey, O.AutoInc)
    def name      = column[Name]     ("name")
    def quantity  = column[Int]      ("quantity")
    def soldByRef = column[StoreId]  ("sold_by")

    def *        = (id, name, quantity, soldByRef) <> (Product.tupled, Product.unapply)

    def soldBy   = foreignKey("product_store", soldByRef, Stores)(_.id)
  }
  val Products = TableQuery[ProductT]

  class EmployeeT(tag: Tag) extends Table[Employee](tag, "employees"){
    val id         = column[EmployeeId]("id", O.PrimaryKey, O.AutoInc)
    val name       = column[Name]      ("name")
    val worksAtRef = column[StoreId]   ("works_at")

    def *          = (id, name, worksAtRef) <> (Employee.tupled, Employee.unapply)

    def worksAt    = foreignKey("employee_store", worksAtRef, Stores)(_.id)
  }
  val Employees  = TableQuery[EmployeeT]
}

trait StoreCrudInstances extends StoreDomain with cellRowInstances {
  /**
   * we need to provide cell instanced for every type we expose through slick-crud,
   *  in order for it to know how to render and parse them
   */
  implicit val c1 = SimpleCell[Name](_.asString, Name)
  implicit val c2 = SimpleCell[Desc](_.asString, Desc)
  implicit val c3 = SimpleCell[StoreId](_.id, StoreId, isEditable = true)
  implicit val c4 = SimpleCell[ProductId](_.id.toString, s ⇒ ProductId(s.toLong))
  implicit val c5 = SimpleCell[EmployeeId](_.id.toString, s ⇒ EmployeeId(s.toLong))

  /**
   * These editable-instances are necessary for now in order to expose
   *  tables that have default projections to a case class for example.
   */
  implicit val e1 = mappedCellRow(Employee.tupled, Employee.unapply)
  implicit val e2 = mappedCellRow(Product.tupled,  Product.unapply)
  implicit val e3 = mappedCellRow(Store.tupled,    Store.unapply)
}


object CrudDemoWebApp extends db.LiquibaseH2TransactionComponent with Plan with LazyLogging {

  class CrudUnfilteredDemo(context: ServletContext) extends StoreTables
                                                    with CrudUnfiltered
                                                    with StoreCrudInstances
                                                    with GenDataModule
                                                    with updateNotifierLogging {
    val profile = CrudDemoWebApp.profile
    val db      = CrudDemoWebApp.db
    val ctx     = context.getContextPath

    import profile.simple._

    db.withTransaction{implicit s ⇒
      Stores    insertAll (GenData.stores    :_*)
      Employees insertAll (GenData.employees :_*)
      Products  insertAll (GenData.products  :_*)
    }

    object notifier extends UpdateNotifierLogging with LazyLogging

    /* sorted by name*/
    private val employees = Editor("/employees", Employees, notifier, isEditable = true)(_.sortBy(_.name.asc), _.id)

    /* tuple projection */
    private val products  = Editor("/products", Products,  notifier)(_.map(t ⇒ (t.id, t.soldByRef, t.quantity, t.name)), _.id)

    /* no custom query, but has foreign keys to employees and products */
    private val stores    = Editor("/stores", Stores, notifier)(identity, _.id).sub(
      employees.on(_.worksAtRef),
      products.on(_.soldByRef)
    )

    override val editors = Seq(employees, products, stores)

    override def respond(title: String)(body: NodeSeq): ResponseFunction[Any] = Html5(PageTemplate.page(ctx, title)(body))
  }

  /* set in init() because that's when we receive context */
  var crudDemo: CrudUnfilteredDemo = _

  override def init(config: FilterConfig): Unit = {
    crudDemo = new CrudUnfilteredDemo(config.getServletContext)
  }

  override def intent = crudDemo.intent
}