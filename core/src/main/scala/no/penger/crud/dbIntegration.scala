package no.penger.crud

trait dbIntegration extends slickIntegration {
  val db:      profile.api.Database
}
