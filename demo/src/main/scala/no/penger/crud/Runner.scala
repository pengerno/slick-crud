package no.penger.crud

object Runner extends App {
  unfiltered.jetty.Server.local(8080).plan(CrudDemoWebApp).run()
}
