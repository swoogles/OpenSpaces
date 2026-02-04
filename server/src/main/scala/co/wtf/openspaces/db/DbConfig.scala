package co.wtf.openspaces.db

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import org.flywaydb.core.Flyway
import zio.*

import javax.sql.DataSource
import scala.util.Try

case class DbConfig(
  jdbcUrl: String,
  user: String,
  password: String
)

object DbConfig:
  /** Parse Heroku-style DATABASE_URL: postgres://user:password@host:port/database */
  private def parseHerokuUrl(url: String): Option[DbConfig] =
    // postgres://user:pass@host:port/db or postgresql://user:pass@host:port/db
    val pattern = """postgres(?:ql)?://([^:]+):([^@]+)@([^:]+):(\d+)/(.+)""".r
    url match
      case pattern(user, password, host, port, database) =>
        Some(DbConfig(
          jdbcUrl = s"jdbc:postgresql://$host:$port/$database",
          user = user,
          password = password
        ))
      case _ => None

  def fromEnv: ZIO[Any, Throwable, DbConfig] =
    System.env("DATABASE_URL").flatMap {
      case Some(url) =>
        // Heroku-style DATABASE_URL
        ZIO.fromOption(parseHerokuUrl(url))
          .mapError(_ => new Exception(s"Failed to parse DATABASE_URL: $url"))
      case None =>
        // Local dev with separate env vars
        for
          host     <- System.env("DB_HOST").someOrElse("localhost")
          port     <- System.env("DB_PORT").someOrElse("5432")
          database <- System.env("DB_NAME").someOrElse("sticky_icky")
          user     <- System.env("DB_USER").someOrElse("postgres")
          password <- System.env("DB_PASSWORD").someOrElse("")
        yield DbConfig(
          jdbcUrl = s"jdbc:postgresql://$host:$port/$database",
          user = user,
          password = password
        )
    }

  val layer: ZLayer[Any, Throwable, DbConfig] =
    ZLayer.fromZIO(fromEnv)

object DataSourceLive:
  def make(config: DbConfig): HikariDataSource =
    val hikariConfig = HikariConfig()
    hikariConfig.setJdbcUrl(config.jdbcUrl)
    hikariConfig.setUsername(config.user)
    hikariConfig.setPassword(config.password)
    // Heroku free tier has limited connections
    hikariConfig.setMaximumPoolSize(5)
    hikariConfig.setMinimumIdle(1)
    hikariConfig.setConnectionTimeout(30000)
    hikariConfig.setIdleTimeout(600000)
    hikariConfig.setMaxLifetime(1800000)
    HikariDataSource(hikariConfig)

  val layer: ZLayer[DbConfig, Throwable, DataSource] =
    ZLayer.scoped:
      for
        config <- ZIO.service[DbConfig]
        ds     <- ZIO.acquireRelease(ZIO.attempt(make(config)))(ds => ZIO.succeed(ds.close()))
      yield ds

object FlywayMigration:
  def migrate(dataSource: DataSource): Task[Unit] =
    ZIO.attempt:
      Flyway
        .configure()
        .dataSource(dataSource)
        .locations("classpath:db/migration")
        .load()
        .migrate()
      ()

  val layer: ZLayer[DataSource, Throwable, Unit] =
    ZLayer.fromZIO:
      for
        ds <- ZIO.service[DataSource]
        _  <- migrate(ds)
        _  <- ZIO.logInfo("Flyway migrations completed")
      yield ()
