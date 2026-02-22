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
  * Avatars stack in columns like Tetris pieces.
  */
object ReplayView:
  // Animation configuration (easy to tweak)
  private val FallDurationMs = 500
  private val DelayBetweenActionsMs = 100
  private val AvatarSize = 20

  // State for a falling/landed avatar
  case class FallingAvatar(
    id: Int,           // Unique ID for tracking
    actor: String,
    actionType: String,
    entityType: String,
    col: Int,          // Column (x position)
    landingRow: Int,   // Row where it will land (from bottom, 0 = bottom row)
  )

  def apply(): Element =
    // State
    val actions: Var[List[ConfirmedActionEntry]] = Var(Nil)
    val avatars: Var[List[FallingAvatar]] = Var(Nil)
    val isPlaying: Var[Boolean] = Var(false)
    val currentIndex: Var[Int] = Var(0)
    val gridDimensions: Var[(Int, Int)] = Var((0, 0)) // (cols, rows)
    val errorMessage: Var[Option[String]] = Var(None)
    val nextId: Var[Int] = Var(0)
    
    // Track how many avatars have landed in each column (for stacking)
    val columnHeights: Var[Map[Int, Int]] = Var(Map.empty)

    // Calculate grid dimensions based on viewport
    def calculateGridDimensions(): (Int, Int) =
      val viewportWidth = dom.window.innerWidth.toInt
      val viewportHeight = dom.window.innerHeight.toInt
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

    // Find a random column that isn't full
    def getRandomAvailableColumn(cols: Int, rows: Int): Option[Int] =
      val heights = columnHeights.now()
      val availableCols = (0 until cols).filter(col => heights.getOrElse(col, 0) < rows).toList
      if availableCols.isEmpty then None
      else
        val idx = (Math.random() * availableCols.size).toInt
        Some(availableCols(idx))

    // Start the animated playback
    def startPlayback(): Unit =
      isPlaying.set(true)
      currentIndex.set(0)
      avatars.set(Nil)
      columnHeights.set(Map.empty)
      nextId.set(0)
      
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
      
      val action = allActions(idx)
      action.actor match
        case Some(actor) =>
          getRandomAvailableColumn(cols, rows) match
            case Some(col) =>
              // Get current height of this column and increment it
              val currentHeight = columnHeights.now().getOrElse(col, 0)
              columnHeights.update(h => h + (col -> (currentHeight + 1)))
              
              // Landing row (from bottom: 0 = bottom, 1 = one above bottom, etc.)
              val landingRow = currentHeight
              
              // Create the avatar
              val id = nextId.now()
              nextId.update(_ + 1)
              
              val avatar = FallingAvatar(
                id = id,
                actor = actor,
                actionType = action.actionType,
                entityType = action.entityType,
                col = col,
                landingRow = landingRow,
              )
              avatars.update(_ :+ avatar)
              
              // Schedule next action (after delay, not after fall completes)
              currentIndex.update(_ + 1)
              setTimeout(DelayBetweenActionsMs.toDouble) {
                if isPlaying.now() then playNextAction()
              }
            case None =>
              // All columns full
              isPlaying.set(false)
        case None =>
          // Skip actions without actors
          currentIndex.update(_ + 1)
          playNextAction()

    // GitHub avatar URL
    def avatarUrl(username: String): String =
      s"https://github.com/$username.png?size=${AvatarSize * 2}"

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
            val withActors = acts.count(_.actor.isDefined)
            s"$idx / $withActors actions"
        },
      ),
      // Grid container - position relative so absolute children work
      div(
        cls := "ReplayView-grid",
        position.relative,
        width := "100%",
        height := "100%",
        overflow.hidden,
        children <-- Signal.combine(avatars.signal, gridDimensions.signal).map {
          case (avs, (cols, rows)) =>
            val viewportHeight = dom.window.innerHeight.toInt
            avs.map { avatar =>
              // Calculate pixel positions
              val xPos = avatar.col * AvatarSize
              // Landing Y: bottom of screen minus (row + 1) * size
              // Row 0 = bottom, so landingY = viewportHeight - AvatarSize
              // Row 1 = one above, so landingY = viewportHeight - 2*AvatarSize
              val landingY = viewportHeight - ((avatar.landingRow + 1) * AvatarSize)
              val startY = -AvatarSize // Start above viewport
              
              div(
                cls := "ReplayView-avatar",
                position.absolute,
                left := s"${xPos}px",
                width := s"${AvatarSize}px",
                height := s"${AvatarSize}px",
                top := s"${startY}px", // Start position
                // Use onMountCallback to trigger animation after render
                onMountCallback { ctx =>
                  val el = ctx.thisNode.ref
                  // Double requestAnimationFrame ensures the initial position is painted first
                  dom.window.requestAnimationFrame { _ =>
                    dom.window.requestAnimationFrame { _ =>
                      el.style.transition = s"top ${FallDurationMs}ms linear"
                      el.style.top = s"${landingY}px"
                    }
                  }
                },
                img(
                  src := avatarUrl(avatar.actor),
                  alt := avatar.actor,
                  title := s"${avatar.actor}: ${avatar.entityType}.${avatar.actionType}",
                  widthAttr := AvatarSize,
                  heightAttr := AvatarSize,
                  borderRadius := "2px",
                ),
              )
            }
        },
      ),
    )
