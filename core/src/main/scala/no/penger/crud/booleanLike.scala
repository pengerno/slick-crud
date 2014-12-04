package no.penger.crud

trait booleanLike {
  sealed trait BooleanLike[A] {
    def isTrue(a: A): Boolean
  }
  implicit object booleanBooleanLike extends BooleanLike[Boolean] {
    override def isTrue(a: Boolean) = a
  }

  implicit object optionBooleanBooleanLike extends BooleanLike[Option[Boolean]] {
    override def isTrue(a: Option[Boolean]) = a.getOrElse(false)
  }
}
