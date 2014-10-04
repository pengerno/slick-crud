package no.penger.crud

trait slickIntegration {
  val profile: slick.driver.JdbcDriver
  def db: profile.simple.Database
}
