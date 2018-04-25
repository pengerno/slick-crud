package no.penger
package crud

import com.typesafe.scalalogging.LazyLogging
import org.joda.time.DateTime
import slick.jdbc.H2Profile
import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.response._

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
    case object Employee extends Role("employee")
    case object Manager extends Role("manager")

    val values = List(Employee, Manager)
    def get(s: String) = s match {
      case Employee.name ⇒ Employee
      case Manager.name  ⇒ Manager
    }
  }
}

trait StoreTables extends StoreDomain with dbIntegration {
  import profile.api._

  /* these type class instances are to enable the use of the types in slick */
  implicit lazy val m1 = MappedColumnType.base[Desc,       String](_.value, Desc)
  implicit lazy val m2 = MappedColumnType.base[EmployeeId, Long]  (  _.id,  EmployeeId)
  implicit lazy val m3 = MappedColumnType.base[Name,       String](_.value, Name)
  implicit lazy val m4 = MappedColumnType.base[ProductId,  Long]  (  _.id,  ProductId)
  implicit lazy val m5 = MappedColumnType.base[StoreId,    String](_.value, StoreId)
  implicit lazy val m6 = MappedColumnType.base[Role,       String](_.name,  Role.get)

  class StoreT(tag: Tag) extends Table[Store](tag, "stores") {
    def id        = column[StoreId]("id")
    def name      = column[Name]   ("name")
    def descr     = column[Option[Desc]]   ("description")
    def closed    = column[Boolean]("closed")
    def *         = (id, name, descr, closed) <> (Store.tupled, Store.unapply)
  }
  val Stores = TableQuery[StoreT]

  class StoreNickNamesT(tag: Tag) extends Table[(StoreId, String)](tag, "store_nicknames"){
    def id = column[StoreId]("id")
    def nickname = column[String]("nickname")
    def * = (id, nickname)
  }
  val StoreNickNames = TableQuery[StoreNickNamesT]

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
    val worksAt   = column[Option[StoreId]]   ("works_at")
    val role      = column[Option[Role]]      ("role")
    val good      = column[Option[Boolean]]   ("good")
    def *         = (id, name, worksAt, role, good) <> (Employee.tupled, Employee.unapply)
  }
  val Employees  = TableQuery[EmployeeT]


  db.run(
    (for {
      _ <- Stores.schema.create
      _ <- StoreNickNames.schema.create
      _ <- Products.schema.create
      _ <- Employees.schema.create
    } yield ()
    ).transactionally
  ).await
}

trait StoreCrudInstances extends StoreDomain with cellRowInstances {
  /**
   * we need to provide cell instances for every type we expose through slick-crud,
   *  in order for it to know how to render and parse them, analogously to slick
   */
  implicit val cellName       = SimpleCell[Name](_.value, Name(_).ensuring(_.value.nonEmpty))
  implicit val cellDesc       = SimpleCell[Desc](_.value, Desc(_).ensuring(_.value.nonEmpty))
  implicit val cellStoreId    = SimpleCell[StoreId](_.value, StoreId(_).ensuring(_.value.nonEmpty), isEditable = true)
  implicit val cellProductId  = SimpleCell[ProductId](_.id.toString, s ⇒ ProductId(s.toLong))
  implicit val cellEmployeeId = SimpleCell[EmployeeId](_.id.toString, s ⇒ EmployeeId(s.toLong))
  implicit val cellRole       = ConstrainedCell[Role](SimpleCell[Role](_.name, Role.get), None)(Some(Role.values))
  /**
   * These cellRow mapping instances are necessary in order to expose
   *  tables that have default projections to non-tuple structures.
   */
  implicit val e1 = mappedCellRow(Employee.tupled, Employee.unapply)
  implicit val e2 = mappedCellRow(Product.tupled,  Product.unapply)
  implicit val e3 = mappedCellRow(Store.tupled,    Store.unapply)
}


/* This demonstrates the wiring that you need to do in your application to get it working */
object CrudDemoWebApp extends Plan with LazyLogging {
  lazy val profile = H2Profile
  import profile.api._

  val db = Database.forURL(
    url    = s"jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
    driver = "org.h2.Driver"
  )

  class CrudUnfilteredDemo(override val ctx: String) extends StoreTables
                                                        with CrudUnfiltered
                                                        with StoreCrudInstances
                                                        with GenDataModule
                                                        with updateNotifierLogging
                                                        with updateNotifierChangelog {

    override lazy val profile = CrudDemoWebApp.profile
    override lazy val db      = CrudDemoWebApp.db

    /* What information to store about a user based on the http request.
     * You could use cookies, auth headers or whatever you want to identify */
    override def userDetails(req: REQ) =
      List(Option(req.underlying.getRemoteUser), Option(req.underlying.getRemoteAddr))
       .flatten.mkString("@")

    /* log changes to log file and to changelog-table */
    object notifier extends UpdateNotifierLogging with UpdateNotifierChangelog with LazyLogging


    /* generate some data to play with */
    db.run(
      (for {
          _ <- Stores ++= GenData.stores
          _ <- StoreNickNames ++= GenData.storeNicknames
          _ <- Employees      ++= GenData.employees
          _ <- Products       ++= GenData.products
        } yield ()
      ).transactionally
    ).await

                        // It's not neccessary to explicitly tag types,
                        // but it makes IDEs behave better if you have
                        // many inter-linked tables
    lazy val storesRef: TableRef[StoreId, StoreT, (Rep[StoreId], Rep[Name], Rep[Option[Desc]], Rep[Boolean], Rep[Option[String]]), (StoreId, Name, Option[Desc], Boolean, Option[String])] =
      TableRef("/stores", Stores, isEditable = true, pageSize = Some(50))(_.id)
        //sort the table by name when we display it
        .projected(_.sortBy(_.name))
        // include a column from another table when we display. This will not
        // affect creating new rows, and the linked columns will not be editable
        .projected(_.joinLeft(StoreNickNames).on(_.id === _.id)
                    .map{ case (s, d) ⇒
                          (s.id, s.name, s.descr, s.closed, d.map(_.nickname))
                        }
                  )

    lazy val employeeRef: TableRef[EmployeeId, EmployeeT, EmployeeT, Employee] =
      TableRef("/employees", Employees, isEditable = true)(_.id)
       .projected(_.sortBy(_.name.asc))
       .linkedOn(_.worksAt, storesRef)(_._1)(_ === _)

    lazy val productsRef: TableRef[ProductId, ProductT, (Rep[ProductId], Rep[StoreId], Rep[Int], Rep[Name]), (ProductId, StoreId, Int, Name)] =
      TableRef("/products",  Products, canDelete = true)(_.id)
       .projected(_.map(t ⇒ (t.id, t.soldBy, t.quantity, t.name)))
       .linkedOn(_._2, storesRef)(_._1)(_ === _)

    lazy val storeNickNamesRef: TableRef[StoreId, StoreNickNamesT, StoreNickNamesT, (StoreId, String)] =
      TableRef("/storeNicknames",  StoreNickNames)(_.id)
       .linkedOn(_.id, storesRef)(_._1)(_ === _)

    //bind to another table on this tables' storeId
    val linkedStoreRef =
      storesRef
        .linkedOn(_._1, employeeRef)(_.worksAt)(_ === _)
        .linkedOn(_._1, productsRef)(_._2)(_ === _)
        .linkedOn(_._1, storeNickNamesRef)(_.id)(_ === _)

    /* also expose a readonly version of the changelog */
    val changeLogRef = {
      db.run(notifier.Changelog.schema.create).await

      implicit val cellTableName = SimpleCell[TableName ](_.toString, TableName)
      implicit val cellColName   = SimpleCell[ColumnName](_.toString, ColumnName)
      implicit val cellDateTime  = SimpleCell[DateTime  ](_.toString, _ ⇒ ???)

      TableRef("/changelog", notifier.Changelog, isEditable = false)(_.id)
    }

    override val editors = Seq(
      Editor(employeeRef,       notifier),
      Editor(productsRef,       notifier),
      Editor(linkedStoreRef,    notifier),
      Editor(storeNickNamesRef, notifier),
      Editor(changeLogRef,      notifier)
    )

    override def respond(title: String)(body: NodeSeq): ResponseFunction[Any] =
      Html5(PageTemplate.page(ctx, title, unFilteredEditors.map {
        e => (e.editor.tableName, e.url.Read)
      })(body))
  }

  lazy val instance = new CrudUnfilteredDemo(config.getServletContext.getContextPath)

  lazy val defaultIntent: Plan.Intent = {
    case ContextPath(_, "/") =>
      instance.respond("Slick-Crud store demo")(
        <h1 style="text-align: center;">Choose a table above</h1>
      )
  }

  override lazy val intent = instance.intent orElse defaultIntent
}

object Runner extends App {
  unfiltered.jetty.Server.local(8080).plan(CrudDemoWebApp).run()
}
