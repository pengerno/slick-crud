package no.penger.crud

import scala.util.Random

trait GenDataModule extends StoreDomain {
  object GenData {
    private def readLines(resourceName: String): Iterator[String] = {
      io.Source.fromInputStream(getClass.getResourceAsStream("/" + resourceName))
        .getLines()
        .map(_.trim)
        .filterNot(_.isEmpty)
    }

    private def extractStores(lines: List[(String, Int)]): List[Store] = lines match {
      case (name, id) :: (description, _) :: tail ⇒
        Store(
          StoreId(id.toString),
          Name(name),
          Some(Desc(description)).filterNot(_ ⇒ Random.nextInt(6) == 0),
          Random.nextBoolean()
        ) :: extractStores(tail)
      case _ ⇒ Nil
    }
    private def howMany = 1 + Random.nextInt(6)

    //thanks to http://grammar.about.com/od/words/a/punnamestores.htm
    val stores = extractStores(readLines("stores.txt").toList.zipWithIndex)

    //listofrandomnames.com
    val names = readLines("names.txt")

    def randomRole = Random.nextInt(3) match {
      case 0 ⇒ None
      case 1 ⇒ Some(Role.Employee)
      case 2 ⇒ Some(Role.Manager)
    }

    def randomGood = Random.nextInt(3) match {
      case 0 ⇒ None
      case 1 ⇒ Some(true)
      case 2 ⇒ Some(false)
    }

    val employees = stores.flatMap { store ⇒
      0 until howMany map { n ⇒
        Employee(EmployeeId(0), Name(names.next()), Option(store.id).filter(_ ⇒ Random.nextBoolean), randomRole, randomGood)
      }
    }
    val products = stores.flatMap { store ⇒
      0 until howMany map { n ⇒
        Product(ProductId(0), Name("product name"), Random.nextInt(500), store.id)
      }
    }
  }
}