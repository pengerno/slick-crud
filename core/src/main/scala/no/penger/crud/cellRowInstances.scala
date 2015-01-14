package no.penger.crud

trait cellRowInstances extends cellInstances {

  /**
   * Use this to use tables mapped to a non-tuple structure.
   **/
  def mappedCellRow[Mapped, Tupled <: Product : CellRow]
                   (apply: Tupled ⇒ Mapped, unapply: Mapped ⇒ Option[Tupled]): CellRow[Mapped] =
    new CellRow[Mapped] {
      val wrapped                           = implicitly[CellRow[Tupled]]
      override def packValues(vs: Seq[Any]) = apply(wrapped.packValues(vs))
      override def unpackValues(e: Mapped)  = wrapped.unpackValues(unapply(e).get)
      override def cells                    = wrapped.cells
    }

  /**
   * Add support for tuples out of the box
   */
  trait CellRowProduct[P <: Product] extends CellRow[P] {
    def unpackValues(e: P): List[Any] = e.productIterator.toList
  }

  private def c[A: Cell]: Cell[A] = implicitly[Cell[A]]
  private def i[T](vs: Seq[Any], idx: Int): T = vs(idx).asInstanceOf[T]

  implicit def singular[A1: Cell]: CellRow[A1] = new CellRow[A1]{
    def cells = List(c[A1])
    def packValues(vs: Seq[Any]) = vs.head.asInstanceOf[A1]
    def unpackValues(e: A1) = List(e)
  }

  implicit def tuple2[A1: Cell, A2: Cell]: CellRowProduct[(A1, A2)] =
    new CellRowProduct[(A1, A2)] {
      def cells = List(c[A1], c[A2])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1))
    }

  implicit def tuple3[A1: Cell, A2: Cell, A3: Cell]: CellRowProduct[(A1, A2, A3)] =
    new CellRowProduct[(A1, A2, A3)] {
      def cells = List(c[A1], c[A2], c[A3])
      def packValues(a: Seq[Any]) =  (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2))
    }

  implicit def tuple4[A1: Cell, A2: Cell, A3: Cell, A4: Cell]: CellRowProduct[(A1, A2, A3, A4)] =
    new CellRowProduct[(A1, A2, A3, A4)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3))
    }

  implicit def tuple5[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell]: CellRowProduct[(A1, A2, A3, A4, A5)] =
    new CellRowProduct[(A1, A2, A3, A4, A5)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4))
    }

  implicit def tuple6[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5))
    }

  implicit def tuple7[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6))
    }

  implicit def tuple8[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7))
    }

  implicit def tuple9[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8))
    }

  implicit def tuple10[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9))
    }

  implicit def tuple11[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10))
    }

  implicit def tuple12[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11))
    }

  implicit def tuple13[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12))
    }

  implicit def tuple14[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13))
    }

  implicit def tuple15[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14))
    }

  implicit def tuple16[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15))
    }

  implicit def tuple17[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16))
    }

  implicit def tuple18[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17))
    }

  implicit def tuple19[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17), i[A19](a, 18))
    }

  implicit def tuple20[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell, A20: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19], c[A20])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17), i[A19](a, 18), i[A20](a, 19))
    }

  implicit def tuple21[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell, A20: Cell, A21: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19], c[A20], c[A21])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17), i[A19](a, 18), i[A20](a, 19), i[A21](a, 20))
    }

  implicit def tuple22[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell, A20: Cell, A21: Cell, A22: Cell]: CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] =
    new CellRowProduct[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19], c[A20], c[A21], c[A22])
      def packValues(a: Seq[Any]) = (i[A1](a, 0), i[A2](a, 1), i[A3](a, 2), i[A4](a, 3), i[A5](a, 4), i[A6](a, 5), i[A7](a, 6), i[A8](a, 7), i[A9](a, 8), i[A10](a, 9), i[A11](a, 10), i[A12](a, 11), i[A13](a, 12), i[A14](a, 13), i[A15](a, 14), i[A16](a, 15), i[A17](a, 16), i[A18](a, 17), i[A19](a, 18), i[A20](a, 19), i[A21](a, 20), i[A22](a, 21))
    }
}