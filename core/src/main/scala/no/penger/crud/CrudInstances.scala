package no.penger.crud

trait CrudInstances extends CRUD {

  /**
   * This trait is used to provide support for tuples out of the box.
   *
   */
  trait ProductEditable[P <: Product] extends Editable[P] {
    def list(e: P): List[Any] = e.productIterator.toList
  }

  /**
   * Use this to use tables mapped to a non-tuple structure.
   *
   * TODO: is there a way this can be provided automatically? shapeless?
   * */
   def mappedEditable[Mapped <: Product, Tupled: Editable]: Editable[Mapped] = new ProductEditable[Mapped]{
    private val wrapped = implicitly[Editable[Tupled]]
    override def cells = wrapped.cells
  }

  import Cell.{from => c}

  implicit def tuple2[A1: Cell, A2: Cell] =
    new ProductEditable[(A1, A2)] {
      def cells = List(c[A1], c[A2])
    }

  implicit def tuple3[A1: Cell, A2: Cell, A3: Cell] =
    new ProductEditable[(A1, A2, A3)] {
      def cells = List(c[A1], c[A2], c[A3])
    }

  implicit def tuple4[A1: Cell, A2: Cell, A3: Cell, A4: Cell] =
    new ProductEditable[(A1, A2, A3, A4)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4])
    }

  implicit def tuple5[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5])
    }

  implicit def tuple6[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6])
    }

  implicit def tuple7[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7])
    }

  implicit def tuple8[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8])
    }

  implicit def tuple9[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9])
    }

  implicit def tuple10[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10])
    }

  implicit def tuple11[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11])
    }

  implicit def tuple12[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12])
    }

  implicit def tuple13[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13])
    }

  implicit def tuple14[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14])
    }

  implicit def tuple15[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15])
    }

  implicit def tuple16[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16])
    }

  implicit def tuple17[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17])
    }

  implicit def tuple18[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18])
    }

  implicit def tuple19[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19])
    }

  implicit def tuple20[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell, A20: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19], c[A20])
    }

  implicit def tuple21[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell, A20: Cell, A21: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19], c[A20], c[A21])
    }

  implicit def tuple22[A1: Cell, A2: Cell, A3: Cell, A4: Cell, A5: Cell, A6: Cell, A7: Cell, A8: Cell, A9: Cell, A10: Cell, A11: Cell, A12: Cell, A13: Cell, A14: Cell, A15: Cell, A16: Cell, A17: Cell, A18: Cell, A19: Cell, A20: Cell, A21: Cell, A22: Cell] =
    new ProductEditable[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] {
      def cells = List(c[A1], c[A2], c[A3], c[A4], c[A5], c[A6], c[A7], c[A8], c[A9], c[A10], c[A11], c[A12], c[A13], c[A14], c[A15], c[A16], c[A17], c[A18], c[A19], c[A20], c[A21], c[A22])
    }
}