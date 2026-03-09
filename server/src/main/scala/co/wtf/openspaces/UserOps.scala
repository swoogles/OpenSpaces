package co.wtf.openspaces

import co.wtf.openspaces.db.UserRepository
import co.wtf.openspaces.github.GitHubProfileService
import zio.*

object UserOps:
  /** Ensure a user exists in the database, fetching GitHub profile if needed. */
  def ensureUserExists(
    username: String,
    userRepo: UserRepository,
    gitHubProfileService: GitHubProfileService,
  ): Task[Unit] =
    if username == "system" then
      userRepo.upsert(username, Some(username)).unit
    else
      gitHubProfileService.ensureUserWithDisplayName(username).unit

  /** Generate a random positive Long ID. */
  def generateId: UIO[Long] =
    zio.Random.nextLong.map(n => if n == Long.MinValue then 0L else math.abs(n))
