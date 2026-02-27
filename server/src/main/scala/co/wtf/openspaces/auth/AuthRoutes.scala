package co.wtf.openspaces.auth

import co.wtf.openspaces.*
import co.wtf.openspaces.discussions.DiscussionActionConfirmed
import co.wtf.openspaces.db.UserRepository
import co.wtf.openspaces.slack.SlackNotifier
import zio.*
import zio.http.*
import zio.json.EncoderOps

case class AuthRoutes(
  adminConfig: AdminConfig,
  userRepository: UserRepository,
  sessionService: SessionService,
  slackNotifier: SlackNotifier,
):
  import neotype.unwrap
  import RandomActionApi._

  val routes: Routes[Any, Response] =
    Routes(
      authStatusGet.implement { username =>
        for
          userOpt <- userRepository.findByUsername(username).orDie
          isAdmin = adminConfig.isAdmin(username)
          approved = userOpt.exists(_.approved) || isAdmin // Admins are always approved
          pendingUsers <- if isAdmin then 
            userRepository.findPendingUsers.map(users => 
              Some(users.map(u => PendingUserInfo(
                u.githubUsername,
                u.displayName,
                u.createdAt.toString
              )).toList)
            ).orDie
          else ZIO.succeed(None)
          approvedUsers <- if isAdmin then
            userRepository.findApprovedUsers.map(users =>
              Some(users.map(u => ApprovedUserInfo(
                u.githubUsername,
                u.displayName
              )).toList)
            ).orDie
          else ZIO.succeed(None)
        yield AuthStatusResponse(
          username = username,
          approved = approved,
          isAdmin = isAdmin,
          pendingUsers = pendingUsers,
          approvedUsers = approvedUsers,
        )
      },

      approveUserPost.implement { case (adminUsername, request) =>
        if !adminConfig.isAdmin(adminUsername) then
          ZIO.succeed(UserActionResult(false, "Unauthorized: Only admins can approve users"))
        else
          (for
            success <- userRepository.approveUser(request.username)
            _ <- ZIO.when(success)(
              // Broadcast to all connected clients
              sessionService.broadcastAuthorizationGranted(request.username)
            )
          yield 
            if success then UserActionResult(true, s"User ${request.username} approved")
            else UserActionResult(false, s"User ${request.username} not found or already approved")
          ).orDie
      },

      revokeUserPost.implement { case (adminUsername, request) =>
        if !adminConfig.isAdmin(adminUsername) then
          ZIO.succeed(UserActionResult(false, "Unauthorized: Only admins can revoke users"))
        else if adminConfig.isAdmin(request.username) then
          ZIO.succeed(UserActionResult(false, "Cannot revoke admin users"))
        else
          (for
            success <- userRepository.revokeUser(request.username)
            _ <- ZIO.when(success)(
              sessionService.broadcastAuthorizationRevoked(request.username)
            )
          yield 
            if success then UserActionResult(true, s"User ${request.username} revoked")
            else UserActionResult(false, s"User ${request.username} not found or already revoked")
          ).orDie
      },

      Method.GET / "api" / "admin" / "topics" ->
        handler { (req: Request) =>
          req.queryParam("adminUsername") match
            case None =>
              ZIO.succeed(
                Response
                  .json(UserActionResult(false, "Missing required query param: adminUsername").toJson)
                  .status(Status.BadRequest),
              )
            case Some(adminUsername) if !adminConfig.isAdmin(adminUsername) =>
              ZIO.succeed(
                Response
                  .json(UserActionResult(false, "Unauthorized: Only admins can list topics").toJson)
                  .status(Status.Forbidden),
              )
            case Some(_) =>
              sessionService
                .listAllTopics
                .map(topics =>
                  Response
                    .json(AdminTopicsResponse(topics).toJson)
                    .status(Status.Ok),
                )
                .catchAll { err =>
                  ZIO.succeed(
                    Response
                      .json(UserActionResult(false, s"List topics failed with server error: ${err.getMessage}").toJson)
                      .status(Status.InternalServerError),
                  )
                }
        },

      Method.POST / "api" / "admin" / "topics" / "delete" ->
        handler { (req: Request) =>
          val adminUsernameOpt = req.queryParam("adminUsername")
          val topicIdOpt = req.queryParam("topicId").flatMap(value => scala.util.Try(value.toLong).toOption)

          (adminUsernameOpt, topicIdOpt) match
            case (None, _) =>
              ZIO.succeed(
                Response
                  .json(UserActionResult(false, "Missing required query param: adminUsername").toJson)
                  .status(Status.BadRequest),
              )
            case (_, None) =>
              ZIO.succeed(
                Response
                  .json(UserActionResult(false, "Missing or invalid required query param: topicId").toJson)
                  .status(Status.BadRequest),
              )
            case (Some(adminUsername), Some(_)) if !adminConfig.isAdmin(adminUsername) =>
              ZIO.succeed(
                Response
                  .json(UserActionResult(false, "Unauthorized: Only admins can delete topics").toJson)
                  .status(Status.Forbidden),
              )
            case (Some(_), Some(topicId)) =>
              sessionService
                .deleteTopicAsAdmin(TopicId(topicId))
                .map { gone =>
                  if gone then
                    Response
                      .json(UserActionResult(true, s"Topic $topicId is now absent (deleted or already gone)").toJson)
                      .status(Status.Ok)
                  else
                    Response
                      .json(UserActionResult(false, s"Delete failed: topic $topicId still present after reconciliation").toJson)
                      .status(Status.InternalServerError)
                }
                .catchAll { err =>
                  ZIO.succeed(
                    Response
                      .json(UserActionResult(false, s"Delete failed with server error: ${err.getMessage}").toJson)
                      .status(Status.InternalServerError),
                  )
                }
        },
    )

object AuthRoutes:
  val layer: ZLayer[AdminConfig & UserRepository & SessionService & SlackNotifier, Nothing, AuthRoutes] =
    ZLayer.fromZIO:
      for
        adminConfig <- ZIO.service[AdminConfig]
        userRepository <- ZIO.service[UserRepository]
        sessionService <- ZIO.service[SessionService]
        slackNotifier <- ZIO.service[SlackNotifier]
      yield AuthRoutes(adminConfig, userRepository, sessionService, slackNotifier)
