package co.wtf.openspaces.components

import animus.*
import com.raquo.laminar.api.L.{*, given}

import co.wtf.openspaces.Person

/** Typeclass for items that can be voted on in a queue.
  *
  * Abstracts the common pattern of:
  * - Showing one unvoted item at a time
  * - Revealing the next item only after voting
  * - Displaying a "viewed" list of voted items
  */
trait Votable[T, Id]:
  extension (item: T)
    def votableId: Id
    def hasVotedOn(user: Person): Boolean
    /** Sort key for ordering unvoted items (lower = shown first) */
    def sortKey: (Long, String)

/** Generic voting queue view that shows one unvoted item at a time.
  *
  * Used by both Topics and Activities to create the "vote to reveal" experience.
  */
object VotingQueueView:

  /** Render a voting queue with one-at-a-time reveal pattern.
    *
    * @param items All items (voted and unvoted)
    * @param currentUser The current user for vote checking
    * @param renderCard Function to render a single card (item, signal, transition) => Element
    * @param renderViewedCard Function to render a card in the "viewed" section
    * @param queueTitle Function from unvoted count to title text
    * @param queueSubtitle Subtitle shown when there are unvoted items
    * @param emptyQueueMessage Message when all items have been voted on
    * @param viewedTitle Title for the "already voted" section
    * @param showSwipeHint Signal for showing the swipe hint animation
    */
  def apply[T, Id](
    items: Signal[List[T]],
    currentUser: Signal[Person],
    renderCard: (T, Signal[T], Option[Transition]) => HtmlElement,
    renderViewedCard: (T, Signal[T], Option[Transition]) => HtmlElement,
    queueTitle: Int => String,
    queueSubtitle: String,
    emptyQueueMessage: String,
    viewedTitle: String,
    showSwipeHint: Signal[Boolean] = Signal.fromValue(false),
  )(using votable: Votable[T, Id]): HtmlElement =
    import votable.*

    val $unvoted: Signal[List[T]] = items.combineWith(currentUser).map { case (all, user) =>
      all.filterNot(_.hasVotedOn(user)).sortBy(_.sortKey)
    }

    val $voted: Signal[List[T]] = items.combineWith(currentUser).map { case (all, user) =>
      all.filter(_.hasVotedOn(user))
    }

    // Only show the first unvoted item
    val $nextUnvoted: Signal[List[T]] = $unvoted.map(_.headOption.toList)

    val $unvotedCount: Signal[Int] = $unvoted.map(_.size)
    val $hasVoted: Signal[Boolean] = $voted.map(_.nonEmpty)

    // Track first unvoted ID for wiggle hint
    val $firstUnvotedId: Signal[Option[Id]] = $nextUnvoted.map(_.headOption.map(_.votableId))

    div(
      cls := "VotingQueueView",
      // Unvoted section - shows one at a time
      div(
        cls := "VotingQueueView-pending",
        h3(
          cls := "TopicSection-title",
          child.text <-- $unvotedCount.map { count =>
            if count == 0 then emptyQueueMessage
            else queueTitle(count)
          },
        ),
        child <-- $unvotedCount.map { count =>
          if count == 0 then emptyNode
          else
            p(
              cls := "TopicSection-subtitle",
              queueSubtitle,
            )
        },
        div(
          cls := "TopicsContainer",
          children <-- $nextUnvoted.splitTransition(_.votableId) {
            (id: Id, item: T, signal: Signal[T], transition: Transition) =>
              val isFirstUnvoted = $firstUnvotedId.map(_.contains(id))
              val shouldWiggle = isFirstUnvoted.combineWith(showSwipeHint).map {
                case (first, hint) => first && hint
              }

              div(
                cls <-- shouldWiggle.map(if _ then "SwipeHintWiggle" else ""),
                renderCard(item, signal, Some(transition)),
              )
          },
        ),
      ),
      // Voted section - shows all voted items
      div(
        cls := "VotingQueueView-viewed",
        h3(cls := "TopicSection-title", viewedTitle),
        child.maybe <-- $hasVoted.map { hasVoted =>
          if hasVoted then None
          else Some(div(cls := "TopicSection-empty", "Nothing here yet."))
        },
        div(
          cls := "TopicsContainer",
          children <-- $voted.splitTransition(_.votableId) {
            (id: Id, item: T, signal: Signal[T], transition: Transition) =>
              renderViewedCard(item, signal, Some(transition))
          },
        ),
      ),
    )

  /** Compute the count of unvoted items for badge display. */
  def unvotedCount[T, Id](
    items: Signal[List[T]],
    currentUser: Signal[Person],
  )(using votable: Votable[T, Id]): Signal[Int] =
    import votable.*
    items.combineWith(currentUser).map { case (all, user) =>
      all.count(!_.hasVotedOn(user))
    }
