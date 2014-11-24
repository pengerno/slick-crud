package no.penger.crud

trait slickIntegration {
  val profile: slick.driver.JdbcDriver
  val db:      profile.simple.Database
}
