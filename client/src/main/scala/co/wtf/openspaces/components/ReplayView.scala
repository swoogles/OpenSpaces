package co.wtf.openspaces.components

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import zio.json.*
import scala.scalajs.js
import scala.scalajs.js.timers.*

import co.wtf.openspaces.{ConfirmedActionEntry, ConfirmedActionsResponse}

/** Tetris-style visualization of conference participation.
  * 
  * Shows GitHub avatars falling into a grid, one action at a time.
  * Each avatar represents a confirmed action by a user.
  */
object ReplayView:
  // Animation configuration (easy to tweak)
  private val FallDurationMs = 500
  private val DelayBetweenActionsMs = 100
  private val AvatarSize = 20

  // State for an avatar in the grid
  case class GridCell(
    actor: String,
    actionType: String,
    entityType: String,
    isFalling: Boolean,
    finalRow: Int,
    finalCol: Int,
  )

  def apply(): Element =
    // State
    val actions: Var[List[ConfirmedActionEntry]] = Var(Nil)
    val grid: Var[List[GridCell]] = Var(Nil)
    val isPlaying: Var[Boolean] = Var(false)
    val currentIndex: Var[Int] = Var(0)
    val gridDimensions: Var[(Int, Int)] = Var((0, 0)) // (cols, rows)
    val errorMessage: Var[Option[String]] = Var(None)

    // Calculate grid dimensions based on viewport
    def calculateGridDimensions(): (Int, Int) =
      val viewportWidth = dom.window.innerWidth.toInt
      val viewportHeight = dom.window.innerHeight.toInt - 100 // Leave room for header
      val cols = viewportWidth / AvatarSize
      val rows = viewportHeight / AvatarSize
      (cols, rows)

    // Fetch confirmed actions from API
    def fetchActions(): Unit =
      val xhr = new dom.XMLHttpRequest()
      xhr.open("GET", "/api/admin/confirmed-actions")
      xhr.onload = { _ =>
        if xhr.status == 200 then
          xhr.responseText.fromJson[ConfirmedActionsResponse] match
            case Right(response) =>
              // Filter to only actions with actors
              val actionsWithActors = response.actions.filter(_.actor.isDefined)
              actions.set(actionsWithActors)
              errorMessage.set(None)
              // Auto-start playback
              if actionsWithActors.nonEmpty then
                startPlayback()
            case Left(error) =>
              errorMessage.set(Some(s"Failed to parse response: $error"))
        else
          errorMessage.set(Some(s"Failed to fetch actions: ${xhr.status}"))
      }
      xhr.onerror = { _ =>
        errorMessage.set(Some("Network error fetching actions"))
      }
      xhr.send()

    // Generate a random position in the grid that isn't taken
    def getRandomPosition(occupiedPositions: Set[(Int, Int)], cols: Int, rows: Int): Option[(Int, Int)] =
      val allPositions = for
        col <- 0 until cols
        row <- 0 until rows
      yield (col, row)
      
      val available = allPositions.filterNot(occupiedPositions.contains).toList
      if available.isEmpty then None
      else
        val idx = (Math.random() * available.size).toInt
        Some(available(idx))

    // Start the animated playback
    def startPlayback(): Unit =
      isPlaying.set(true)
      currentIndex.set(0)
      grid.set(Nil)
      
      val dims = calculateGridDimensions()
      gridDimensions.set(dims)
      
      playNextAction()

    // Play the next action in the sequence
    def playNextAction(): Unit =
      val idx = currentIndex.now()
      val allActions = actions.now()
      val (cols, rows) = gridDimensions.now()
      
      if idx >= allActions.size then
        isPlaying.set(false)
        return
      
      val currentGrid = grid.now()
      val occupiedPositions = currentGrid.map(c => (c.finalCol, c.finalRow)).toSet
      
      // Check if grid is full
      if occupiedPositions.size >= cols * rows then
        isPlaying.set(false)
        return
      
      val action = allActions(idx)
      action.actor match
        case Some(actor) =>
          getRandomPosition(occupiedPositions, cols, rows) match
            case Some((col, row)) =>
              // Add the new cell in falling state
              val newCell = GridCell(
                actor = actor,
                actionType = action.actionType,
                entityType = action.entityType,
                isFalling = true,
                finalRow = row,
                finalCol = col,
              )
              grid.update(_ :+ newCell)
              
              // After fall duration, mark as landed
              setTimeout(FallDurationMs.toDouble) {
                grid.update(_.map { cell =>
                  if cell.finalCol == col && cell.finalRow == row then
                    cell.copy(isFalling = false)
                  else cell
                })
              }
              
              // Schedule next action
              currentIndex.update(_ + 1)
              setTimeout(DelayBetweenActionsMs.toDouble) {
                if isPlaying.now() then playNextAction()
              }
            case None =>
              // Grid is full
              isPlaying.set(false)
        case None =>
          // Skip actions without actors
          currentIndex.update(_ + 1)
          playNextAction()

    // GitHub avatar URL
    def avatarUrl(username: String): String =
      s"https://github.com/$username.png?size=$AvatarSize"

    div(
      cls := "ReplayView",
      // Fetch actions on mount
      onMountCallback { _ =>
        fetchActions()
      },
      // Error display
      child.maybe <-- errorMessage.signal.map(_.map { error =>
        div(cls := "ReplayView-error", error)
      }),
      // Progress indicator
      div(
        cls := "ReplayView-progress",
        child.text <-- Signal.combine(currentIndex.signal, actions.signal).map {
          case (idx, acts) =>
            val total = acts.size
            val withActors = acts.count(_.actor.isDefined)
            s"$idx / $withActors actions (${acts.size} total)"
        },
      ),
      // Grid container
      div(
        cls := "ReplayView-grid",
        children <-- Signal.combine(grid.signal, gridDimensions.signal).map {
          case (cells, (cols, rows)) =>
            cells.map { cell =>
              val finalTop = cell.finalRow * AvatarSize
              div(
                cls := "ReplayView-avatar",
                // Start at top of screen, will animate to final position
                position.absolute,
                left := s"${cell.finalCol * AvatarSize}px",
                width := s"${AvatarSize}px",
                height := s"${AvatarSize}px",
                top := s"${-AvatarSize}px",
                Modifier { el =>
                  // After mount, trigger animation on next frame
                  dom.window.requestAnimationFrame { _ =>
                    el.ref.style.transition = s"top ${FallDurationMs}ms linear"
                    el.ref.style.top = s"${finalTop}px"
                  }
                },
                img(
                  src := avatarUrl(cell.actor),
                  alt := cell.actor,
                  title := s"${cell.actor}: ${cell.entityType}.${cell.actionType}",
                  widthAttr := AvatarSize,
                  heightAttr := AvatarSize,
                ),
              )
            }
        },
      ),
    )
