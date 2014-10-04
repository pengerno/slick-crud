package no.penger
package crud


trait editables extends cells {
  @annotation.implicitNotFound("Couldnt find cell instances for all the types in projection ${PROJECTION}")
  trait Editable[PROJECTION]{
    def cells:List[Cell[_]]
    def list(e:PROJECTION):List[Any]
  }
}
