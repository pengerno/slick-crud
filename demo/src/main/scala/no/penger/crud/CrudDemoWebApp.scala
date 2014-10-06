package no.penger
package crud

import com.typesafe.scalalogging.slf4j.LazyLogging
import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request.GET
import unfiltered.response._

import scala.language.implicitConversions
import scala.xml.NodeSeq

trait StoreDomain{
  case class Name(asString: String)
  case class Desc(asString: String)

  case class StoreId(id: String)
  case class Store(id: StoreId, name: Name, description: Option[Desc])

  case class ProductId(id: Long)
  case class Product(id: ProductId, name: Name, quantity: Int, soldBy: StoreId)

  case class EmployeeId(id: Long)
  case class Employee(id: EmployeeId, name: Name, worksAt: StoreId)
}

trait StoreTables extends StoreDomain with db.SlickTransactionBoundary {
  import profile.simple._

  implicit lazy val m1 = MappedColumnType.base[Desc,       String](_.asString, Desc)
  implicit lazy val m2 = MappedColumnType.base[EmployeeId, Long](  _.id,       EmployeeId)
  implicit lazy val m3 = MappedColumnType.base[Name,       String](_.asString, Name)
  implicit lazy val m4 = MappedColumnType.base[ProductId,  Long](  _.id,       ProductId)
  implicit lazy val m5 = MappedColumnType.base[StoreId,    String](_.id,       StoreId)

  class StoreT(tag: Tag) extends Table[Store](tag, "stores") {
    def id       = column[StoreId]("id")
    def name     = column[Name]   ("name")
    def address  = column[Desc]   ("description").?

    def *        = (id, name, address) <> (Store.tupled, Store.unapply)

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

trait StoreCrudPlan extends StoreTables with Crud with logging.updateNotifierLogging {
  object crudPlan extends CrudPlan {

    /**
     * we need to provide cell instanced for every type we expose through slick-crud,
     *  in order for it to know how to render and parse them
     */
    implicit val c1 = Cell[Name](_.asString, Name)
    implicit val c2 = Cell[Desc](_.asString, Desc)
    implicit val c3 = Cell[StoreId](_.id, StoreId, canEdit = true)
    implicit val c4 = Cell[ProductId](_.id.toString, s => ProductId(s.toLong), canEdit = false)
    implicit val c5 = Cell[EmployeeId](_.id.toString, s => EmployeeId(s.toLong), canEdit = false)

    /**
     * These editable-instances are necessary for now in order to expose
     *  tables that have default projections to a case class for example.
     */
    implicit val e1 = mappedCellRow(Employee.unapply)
    implicit val e2 = mappedCellRow(Product.unapply)
    implicit val e3 = mappedCellRow(Store.unapply)

    object notifier extends UpdateNotifierLogging with LazyLogging

    /* not editable, sorted by name*/
    private val employees = Editor(Employees, notifier, isEditable = false)(_.sortBy(_.name.asc), _.id)

    /* tuple projection */
    private val products  = Editor(Products,  notifier)(_.map(t => (t.id, t.soldByRef, t.name)), _.id)

    /* no custom query, but has foreign keys to employees and products */
    private val stores    = Editor(Stores, notifier)(identity, _.id).sub(
      employees.on(_.worksAtRef),
      products.on(_.soldByRef)
      //todo: single something
    )

    override val editors = Map(
      "/employees" → employees,
      "/products"  → products,
      "/stores"    → stores
    )

    override val resourceIntent: Plan.Intent = {
      /* dont do this at home etc */
      case req@GET(ContextPath(ctx, resource)) =>
        val optStream = Option(classOf[StoreCrudPlan].getResourceAsStream(resource))
        optStream.fold[ResponseFunction[Any]](NotFound) (
          is => Ok ~> ResponseString(io.Source.fromInputStream(is).getLines().mkString("\n"))
        )
    }
  }

  override def respond(ctx: String, title: String)(body: NodeSeq): ResponseFunction[Any] = Html5(PageTemplate.page(ctx, title)(body))
}

object CrudDemoWebApp
  extends StoreCrudPlan
  with GenDataModule
  with db.LiquibaseH2TransactionComponent
  with Plan
  with LazyLogging {

  override lazy val intent = crudPlan.intent

  import profile.simple._

  transaction.readWrite{implicit tx =>
    Stores    insertAll (GenData.stores :_*)
    Employees insertAll (GenData.employees :_*)
    Products  insertAll (GenData.products :_*)
 }
}