package no.penger.crud

import no.penger.db.{LiquibaseH2TransactionComponent, SlickTransactionBoundary}
import unfiltered.filter.Plan

import scala.language.implicitConversions
import scala.util.Random

trait StoreDomain{
  case class Name(asString: String)
  case class Desc(asString: String)

  case class StoreId(id: String)
  case class Store(id: StoreId, name: Name, description: Option[Desc])

//  case class StoreParams(isFancy: Boolean, )

  case class ProductId(id: Long)
  case class Product(id: ProductId, name: Name, quantity: Int, soldBy: StoreId)

  case class EmployeeId(id: Long)
  case class Employee(id: EmployeeId, name: Name, worksAt: StoreId)
}

trait StoreTables extends StoreDomain with SlickTransactionBoundary {
  import profile.simple._

  implicit lazy val m1 = MappedColumnType.base[Desc,       String](_.asString, Desc)
  implicit lazy val m2 = MappedColumnType.base[EmployeeId, Long](  _.id,       EmployeeId)
  implicit lazy val m3 = MappedColumnType.base[Name,       String](_.asString, Name)
  implicit lazy val m4 = MappedColumnType.base[ProductId,  Long](  _.id,       ProductId)
  implicit lazy val m5 = MappedColumnType.base[StoreId,    String](_.id,       StoreId)

  class StoreT(tag: Tag) extends Table[Store](tag, "stores") {
    def id       = column[StoreId]     ("id")
    def name     = column[Name]        ("name")
    def address  = column[Option[Desc]]("description")

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

    def soldBy   = foreignKey("product_store", soldByRef, Stores)(_.id)
    def *         = (id, name, quantity, soldByRef) <> (Product.tupled, Product.unapply)
  }
  val Products = TableQuery[ProductT]
  
  class EmployeeT(tag: Tag) extends Table[Employee](tag, "employees"){
    def id         = column[EmployeeId]("id", O.PrimaryKey, O.AutoInc)
    def name       = column[Name]      ("name")
    def worksAtRef = column[StoreId]   ("works_at")

    def worksAt    = foreignKey("employee_store", worksAtRef, Stores)(_.id)
    def *          = (id, name, worksAtRef) <> (Employee.tupled, Employee.unapply)
  }
  val Employees  = TableQuery[EmployeeT]
}

trait StoreCrudPlan extends StoreTables with CrudInstances {

  implicit val c1 = Cell[Name](_.asString, Name)
  implicit val c2 = Cell[Desc](_.asString, Desc)
  implicit val c3 = Cell[StoreId](_.id, StoreId, canEdit = true)
  implicit val c4 = Cell[ProductId](_.id.toString, s => ProductId(s.toLong), canEdit = false)
  implicit val c5 = Cell[EmployeeId](_.id.toString, s => EmployeeId(s.toLong), canEdit = false)

  implicit val e1 = mappedEditable[Employee, (EmployeeId, Name, StoreId)]
  implicit val e2 = mappedEditable[Product,  (ProductId,  Name, Int, StoreId)]
  implicit val e3 = mappedEditable[Store,    (StoreId,    Name, Option[Desc])]

  private lazy val employees = Editor(Employees, "/employees")(identity)(key = _.id)
  private lazy val products  = Editor(Products, "/products")(identity)(key = _.id)

  private lazy val stores    = Editor(Stores, "/stores")(identity)(key = _.id).sub(
    employees.on(_.worksAtRef),
    products.on(_.soldByRef)
    //todo: single something
  )

  object crudPlan extends Plan {
    val intent = employees.intent orElse products.intent orElse stores.intent
  }
}

class CrudDemoWebApp extends StoreCrudPlan with LiquibaseH2TransactionComponent with Plan {
  override lazy val intent = crudPlan.intent

  object GenData {
    private def readLines(resourceName: String): Iterator[String] = {
      io.Source.fromInputStream(classOf[CrudDemoWebApp].getResourceAsStream("/" + resourceName))
        .getLines()
        .map(_.trim)
        .filterNot(_.isEmpty)
    }

    private def extractStores(lines: List[(String, Int)]): List[Store] = lines match {
      case (name, id) :: (description, _) :: tail =>
        Store(
          StoreId(id.toString),
          Name(name),
          Some(Desc(description)).filterNot(_ => Random.nextInt(6) == 0)
        ) :: extractStores(tail)
      case _ => Nil
    }
    private def howMany = 1 + Random.nextInt(6)

    //thanks to http://grammar.about.com/od/words/a/punnamestores.htm
    val stores = extractStores(readLines("stores.txt").toList.zipWithIndex)
    //listofrandomnames.com
    val names = readLines("names.txt")

    val employees = stores.flatMap { store =>
      0 until howMany map { n =>
        Employee(EmployeeId(0), Name(names.next), store.id)
      }
    }
    val products = stores.flatMap { store =>
      0 until howMany map { n =>
        Product(ProductId(0), Name("product name"), Random.nextInt(500), store.id)
      }
    }
  }

  import profile.simple._

  transaction.readWrite{implicit tx =>
    Stores insertAll (GenData.stores :_*)
    Employees insertAll (GenData.employees :_*)
    Products insertAll (GenData.products :_*)
 }
}