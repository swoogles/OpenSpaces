package co.wtf.openspaces

import co.wtf.openspaces.db.{UserRepository, UserRow}
import zio.*
import zio.http.*
import zio.json.*

case class GitHubPublicUser(
  name: Option[String] = None,
) derives JsonCodec

trait GitHubProfileService:
  def ensureUserWithDisplayName(username: String): Task[UserRow]

class GitHubProfileServiceLive(
  client: Client,
  userRepository: UserRepository,
) extends GitHubProfileService:

  private def normalized(value: Option[String]): Option[String] =
    value.map(_.trim).filter(_.nonEmpty)

  private def fetchGitHubDisplayName(username: String): Task[Option[String]] =
    ZIO.scoped:
      for
        response <- client
          .addHeader(Header.Accept(MediaType.application.json))
          .addHeader(Header.Custom("User-Agent", "openspaces-app"))
          .url(URL.decode("https://api.github.com").getOrElse(throw new Exception("Bad GitHub API URL")))
          .get(s"/users/$username")
        body <- response.body.asString
        user <- ZIO
          .fromEither(body.fromJson[GitHubPublicUser])
          .mapError(e => new Exception(s"Failed to parse GitHub user profile for $username: $e"))
      yield normalized(user.name)

  def ensureUserWithDisplayName(username: String): Task[UserRow] =
    for
      existing <- userRepository.findByUsername(username)
      resolvedDisplayName <- existing.flatMap(row => normalized(row.displayName)) match
        case Some(value) =>
          ZIO.succeed(Some(value))
        case None =>
          fetchGitHubDisplayName(username)
            .tapError(err => ZIO.logWarning(s"GitHub display-name lookup failed for $username: ${err.getMessage}"))
            .orElseSucceed(None)
            .map(name => name.orElse(Some(username)))
      _ <- userRepository.upsert(username, resolvedDisplayName)
      persisted <- userRepository
        .findByUsername(username)
        .someOrFail(new Exception(s"Failed to persist user $username"))
    yield persisted

object GitHubProfileService:
  val layer: ZLayer[Client & UserRepository, Nothing, GitHubProfileService] =
    ZLayer.fromFunction((client: Client, userRepository: UserRepository) =>
      GitHubProfileServiceLive(client, userRepository),
    )

