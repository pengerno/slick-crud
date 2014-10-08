package no.penger.crud

trait databaseIntegration {
  val profile: slick.driver.JdbcDriver
  def db: profile.simple.Database
}
