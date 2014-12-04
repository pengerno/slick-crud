package no.penger.crud

import scala.language.implicitConversions

trait lower extends cells {
  
  /**
   * handling of optional values.
   * Keep it in 'lowerPriority' to allow implementors to
   *  override this to define an explicit Cell[Option[T]]
   */
  implicit def optionCell[A](implicit wrapped: Cell[A]): Cell[Option[A]] = OptionCell(wrapped)
}

trait cellInstances extends lower {
  implicit val doubleCell     = SimpleCell[Double] (_.toString, _.toDouble,  inputType = "number")
  implicit val intCell        = SimpleCell[Int]    (_.toString, _.toInt,     inputType = "number")
  implicit val longCell       = SimpleCell[Long]   (_.toString, _.toLong,    inputType = "number")
  implicit val stringCell     = SimpleCell[String] (identity,   _.ensuring(_.nonEmpty))
  implicit val booleanCell    = SelectCell[Boolean](_.toString,  _.toBoolean)
  implicit val optBooleanCell = OptionCell(EnumCell(SimpleCell[Boolean](_.toString, _.toBoolean), List(false, true)))
}
