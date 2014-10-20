package no.penger.crud

trait databaseIntegration {
  val profile: slick.driver.JdbcDriver
  val db:      profile.simple.Database
}
