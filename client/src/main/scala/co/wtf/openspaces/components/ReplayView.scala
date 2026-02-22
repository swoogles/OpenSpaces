package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import zio.json.*

import scala.scalajs.js.timers.*

import co.wtf.openspaces.{ConfirmedActionEntry, ConfirmedActionsResponse}

/** Tetris-style visualization of conference participation.
  *
  * Replays confirmed actions by dropping actor avatars into random columns.
  * Avatars fall smoothly from above the screen and stack from the bottom.
  */
object ReplayView:
  private val FallDurationMs = 700
  private val DelayBetweenActionsMs = 120
  private val BoardResetDelayMs = 220
  private val AvatarSizePx = 22

  case class FallingAvatar(
    id: Int,
    actor: String,
    actionLabel: String,
    col: Int,
    landingRow: Int,
    dropDistancePx: Int,
  )

  def apply(): Element =
    val actions = Var(List.empty[ConfirmedActionEntry])
    val avatars = Var(List.empty[FallingAvatar])
    val isPlaying = Var(false)
    val currentIndex = Var(0)
    val errorMessage = Var(Option.empty[String])

    val nextId = Var(0)
    val boardDimensions = Var((1, 1)) // (cols, rows)
    val columnHeights = Var(Map.empty[Int, Int])

    var playbackTimer: Option[SetTimeoutHandle] = None

    def clearPlaybackTimer(): Unit =
      playbackTimer.foreach(clearTimeout)
      playbackTimer = None

    def scheduleNext(delayMs: Int)(f: => Unit): Unit =
      clearPlaybackTimer()
      playbackTimer = Some(setTimeout(delayMs.toDouble)(f))

    def calculateBoardDimensions(): (Int, Int) =
      val width = dom.window.innerWidth.toInt.max(AvatarSizePx)
      val height = dom.window.innerHeight.toInt.max(AvatarSizePx)
      val cols = (width / AvatarSizePx).max(1)
      val rows = (height / AvatarSizePx).max(1)
      (cols, rows)

    def getRandomAvailableColumn(cols: Int, rows: Int): Option[Int] =
      val heights = columnHeights.now()
      val available = (0 until cols).filter(col => heights.getOrElse(col, 0) < rows).toVector
      if available.isEmpty then None
      else
        val idx = (Math.random() * available.size).toInt
        Some(available(idx))

    def resetBoardState(): Unit =
      avatars.set(Nil)
      columnHeights.set(Map.empty)

    def spawnAvatar(action: ConfirmedActionEntry): Boolean =
      val (cols, rows) = boardDimensions.now()
      val actor = action.actor.getOrElse("")
      if actor.isBlank then
        false
      else
        getRandomAvailableColumn(cols, rows) match
          case None =>
            false
          case Some(col) =>
            val landingRow = columnHeights.now().getOrElse(col, 0)
            columnHeights.update(heights => heights + (col -> (landingRow + 1)))

            val id = nextId.now()
            nextId.update(_ + 1)

            val actionLabel = s"${action.entityType}.${action.actionType}"
            val dropDistance = rows * AvatarSizePx + AvatarSizePx

            avatars.update(_ :+ FallingAvatar(
              id = id,
              actor = actor,
              actionLabel = actionLabel,
              col = col,
              landingRow = landingRow,
              dropDistancePx = dropDistance,
            ))
            true

    def playNextAction(): Unit =
      if !isPlaying.now() then
        return

      val idx = currentIndex.now()
      val all = actions.now()

      if idx >= all.size then
        isPlaying.set(false)
        clearPlaybackTimer()
        return

      val action = all(idx)

      action.actor match
        case Some(actor) if actor.nonEmpty =>
          if spawnAvatar(action) then
            currentIndex.update(_ + 1)
            scheduleNext(DelayBetweenActionsMs) {
              playNextAction()
            }
          else
            // Board is full: clear and continue replay from the same action.
            resetBoardState()
            scheduleNext(BoardResetDelayMs) {
              playNextAction()
            }

        case _ =>
          // Skip actions without actor info.
          currentIndex.update(_ + 1)
          scheduleNext(DelayBetweenActionsMs) {
            playNextAction()
          }

    def startPlayback(): Unit =
      clearPlaybackTimer()
      boardDimensions.set(calculateBoardDimensions())
      resetBoardState()
      nextId.set(0)
      currentIndex.set(0)
      isPlaying.set(true)
      playNextAction()

    def fetchActions(): Unit =
      val xhr = new dom.XMLHttpRequest()
      xhr.open("GET", "/api/admin/confirmed-actions")
      xhr.onload = { _ =>
        if xhr.status == 200 then
          xhr.responseText.fromJson[ConfirmedActionsResponse] match
            case Right(response) =>
              val filtered = response.actions.filter(_.actor.exists(_.nonEmpty))
              actions.set(filtered)
              errorMessage.set(None)
              if filtered.nonEmpty then
                startPlayback()
            case Left(err) =>
              errorMessage.set(Some(s"Failed to parse response: $err"))
        else
          errorMessage.set(Some(s"Failed to fetch actions: ${xhr.status}"))
      }
      xhr.onerror = { _ =>
        errorMessage.set(Some("Network error fetching actions"))
      }
      xhr.send()

    def avatarUrl(username: String): String =
      s"https://github.com/$username.png?size=${AvatarSizePx * 2}"

    div(
      cls := "ReplayView",
      onMountCallback { _ =>
        fetchActions()
      },
      onUnmountCallback { _ =>
        isPlaying.set(false)
        clearPlaybackTimer()
      },
      child.maybe <-- errorMessage.signal.map(_.map(error =>
        div(cls := "ReplayView-error", error)
      )),
      div(
        cls := "ReplayView-progress",
        child.text <-- Signal.combine(currentIndex.signal, actions.signal).map {
          case (idx, all) => s"$idx / ${all.size} actions"
        },
      ),
      div(
        cls := "ReplayView-grid",
        onMountCallback { _ =>
          boardDimensions.set(calculateBoardDimensions())
        },
        windowEvents(_.onResize) --> Observer { _ =>
          boardDimensions.set(calculateBoardDimensions())
        },
        children <-- avatars.signal.split(_.id) { (_, initial, avatarSignal) =>
          div(
            cls := "ReplayView-avatar",
            position.absolute,
            left := s"${initial.col * AvatarSizePx}px",
            bottom := s"${initial.landingRow * AvatarSizePx}px",
            width := s"${AvatarSizePx}px",
            height := s"${AvatarSizePx}px",
            transform := s"translateY(-${initial.dropDistancePx}px)",
            onMountCallback { ctx =>
              val el = ctx.thisNode.ref
              dom.window.requestAnimationFrame { _ =>
                dom.window.requestAnimationFrame { _ =>
                  el.style.transition = s"transform ${FallDurationMs}ms linear"
                  el.style.transform = "translateY(0px)"
                }
              }
            },
            child <-- avatarSignal.map { avatar =>
              img(
                src := avatarUrl(avatar.actor),
                alt := avatar.actor,
                title := s"${avatar.actor}: ${avatar.actionLabel}",
                widthAttr := AvatarSizePx,
                heightAttr := AvatarSizePx,
                borderRadius := "3px",
              )
            },
          )
        },
      ),
    )
