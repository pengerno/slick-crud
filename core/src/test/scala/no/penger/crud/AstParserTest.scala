package no.penger.crud

import org.scalatest.FunSuite
import slick.ast.ColumnOption
import slick.jdbc.PostgresProfile

class AstParserTest
  extends FunSuite
  with astParser
  with slickIntegration {

  override val profile = PostgresProfile
  import profile.api._

  def myAssert[E, U, R](q: Query[E, U, Seq], shouldEqual: R)(op: Query[E, U, Seq] ⇒ R) = {
    assertResult(shouldEqual, q.result)(op(q))
  }
  def c(s: String, options: ColumnOption[_]*) = {
    val parts = s.split("\\.")
    ColumnInfo(TableName(parts(0)), ColumnName(parts(1)), options)
  }

  class OneTwoThreeT(tag: Tag) extends Table[(Int, Option[Int], Option[Int])](tag, "t") {
    def one   = column[Int]("one")
    def two   = column[Option[Int]]("two")
    def three = column[Int]("three").?

    def * = (one, two, three)
  }

  case class Strings(s1: String, os1: Option[String], os2: Option[String])
  class OneTwoThreeST(tag: Tag) extends Table[Strings](tag, "t") {
    def one   = column[String]("one")
    def two   = column[Option[String]]("two")
    def three = column[String]("three").?

    def * = (one, two, three) <> (Strings.tupled, Strings.unapply)
  }

  test("understand simple columns"){
    myAssert(TableQuery[OneTwoThreeT], Seq(c("t.one"), c("t.two"), c("t.three")))(AstParser.colNames)
  }

  test("understand map to one column"){
    myAssert(TableQuery[OneTwoThreeT].map(_.two), Seq(c("t.two")))(AstParser.colNames)
  }

  test("understand map to two columns"){
    myAssert(TableQuery[OneTwoThreeT].map(t ⇒ (t.two, t.three)), Seq(c("t.two"), c("t.three")))(AstParser.colNames)
  }

  test("understand map / nested projections"){
    myAssert(TableQuery[OneTwoThreeT].sortBy(_.two).map(t ⇒ (t.two, t.three)).map(_._1), Seq(c("t.two")))(AstParser.colNames)
  }

  test("understand case class projection"){
    myAssert(TableQuery[OneTwoThreeST], Seq(c("t.one"), c("t.two"), c("t.three")))(AstParser.colNames)
  }

  test("understand query"){
    myAssert(TableQuery[OneTwoThreeST].sortBy(_.two.asc), Seq(c("t.one"), c("t.two"), c("t.three")))(AstParser.colNames)
  }

  test("understand query with join"){
    class TT(tag: Tag) extends Table[String](tag, "t2") {
      def one   = column[String]("one")
      def * = one
    }
    myAssert(TableQuery[OneTwoThreeST].join(TableQuery[TT]).on(_.one === _.one).map(_._1),
             Seq(c("t.one"), c("t.two"), c("t.three")))(AstParser.colNames)
  }

  test("join 2x"){
    class OneT(tag: Tag) extends Table[(String, Option[Int])](tag, "t1") {
      def one   = column[String]("one")
      def two   = column[Option[Int]]("two")
      def *     = (one, two)
    }

    class TwoT(tag: Tag) extends Table[(String, Option[String])](tag, "t2") {
      def three = column[String]("three")
      def four  = column[Option[String]]("four")
      def *     = (three, four)
    }
    val One = TableQuery[OneT]
    val Two = TableQuery[TwoT]
    val q1 = One.join(Two).on(_.one === _.three).map{case (one, two) ⇒ (one.one, two.four)}
    val q2 = Two.join(One).on(_.three === _.one).drop(2).sortBy(_._2.one).map{case (two, one) ⇒ (two.four, one.one)}

    myAssert(q1, Seq(c("t1.one"),  c("t2.four")))(AstParser.colNames)
    myAssert(q2, Seq(c("t2.four"), c("t1.one")))(AstParser.colNames)
  }

  test("join 3x"){
    class OneT(tag: Tag) extends Table[(String, Option[Int])](tag, "t1") {
      def one   = column[String]("one")
      def two   = column[Option[Int]]("two")
      def *     = (one, two)
    }

    class TwoT(tag: Tag) extends Table[(String, Option[String])](tag, "t2") {
      def three = column[String]("three")
      def four  = column[Option[String]]("four")
      def *     = (three, four)
    }

    class ThreeT(tag: Tag) extends Table[(String, Option[String])](tag, "t3") {
      def five = column[String]("five")
      def six  = column[Option[String]]("six")
      def *     = (five, six)
    }
    val One   = TableQuery[OneT]
    val Two   = TableQuery[TwoT]
    val Three = TableQuery[ThreeT]

    val q1 = One.join(Two).on(_.one === _.three).join(Three).on(_._1.one === _.five).map{
      case ((one, two), three) ⇒ (one.one, two.four, three.six)
    }

    myAssert(q1, Seq(c("t1.one"),  c("t2.four"), c("t3.six")))(AstParser.colNames)
  }

  test("join 3x full"){
    class OneT(tag: Tag) extends Table[(String, Option[Int])](tag, "t1") {
      def one   = column[String]("one")
      def two   = column[Option[Int]]("two")
      def *     = (one, two)
    }

    class TwoT(tag: Tag) extends Table[(String, Option[String])](tag, "t2") {
      def three = column[String]("three")
      def four  = column[Option[String]]("four")
      def *     = (three, four)
    }

    class ThreeT(tag: Tag) extends Table[(String, Option[String])](tag, "t3") {
      def five = column[String]("five")
      def six  = column[Option[String]]("six")
      def *     = (five, six)
    }
    val One   = TableQuery[OneT]
    val Two   = TableQuery[TwoT]
    val Three = TableQuery[ThreeT]

    val q1 = One.joinFull(Two).on(_.one === _.three).joinRight(Three.sortBy(_.five)).on(_._1.map(_.one) === _.five).map{
      case (onetwo, three) ⇒ (onetwo.flatMap(_._1.map(_.one)), onetwo.flatMap(_._2.map(_.four)), three.six)
    }

    myAssert(q1, Seq(c("t1.one"),  c("t2.four"), c("t3.six")))(AstParser.colNames)
  }

  test("joinLeft"){
    class OneT(tag: Tag) extends Table[(String, Option[Int])](tag, "t1") {
      def one   = column[String]("one")
      def two   = column[Option[Int]]("two")
      def *     = (one, two)
    }

    class TwoT(tag: Tag) extends Table[(String, Option[String])](tag, "t2") {
      def three = column[String]("three")
      def four  = column[Option[String]]("four")
      def *     = (three, four)
    }
    val One = TableQuery[OneT]
    val Two = TableQuery[TwoT]
    val q1 = One.joinLeft(Two).on(_.one === _.three).map{case (one, two) ⇒ (one.one, two.map(_.four))}
    val q2 = Two.joinLeft(One).on(_.three === _.one).drop(2).sortBy(_._2.map(_.one).asc).map{case (two, one) ⇒ (two.four, one.map(_.one))}

    myAssert(q1, Seq(c("t1.one"),  c("t2.four")))(AstParser.colNames)
    myAssert(q2, Seq(c("t2.four"), c("t1.one")))(AstParser.colNames)
  }

  test("column options"){
    class TT(tag: Tag) extends Table[Option[String]](tag, "t") {
      def one   = column[Option[String]]("one", ColumnOption.PrimaryKey)
      override def * = one
    }
    val T = TableQuery[TT]
    myAssert(T, Seq(c("t.one", ColumnOption.PrimaryKey)))(AstParser.colNames)
  }
}