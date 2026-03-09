package co.wtf.openspaces.components

import animus.*
import com.raquo.laminar.api.L.{*, given}
import co.wtf.openspaces.{AppState, Person}

/** Unified card component following TopicCard patterns.
  *
  * Structure:
  * - VoteIndicator (left side) - shows interest status and member count
  * - Content (center) - entity-specific content via slot
  * - Actions (right side) - entity-specific actions via slot
  * - Expandable member list (bottom) - shows avatars when indicator is clicked
  *
  * This component provides the consistent visual structure of TopicCard
  * while allowing different entity types to customize their content.
  */
object EntityCard:
  /** Swipe action type - entities must provide their own enum but map to this for rendering */
  enum SwipeDirection:
    case Left, Right

  case class Config(
    /** Unique ID for tracking expanded state across cards */
    entityKey: String,
    /** Is the current user interested in this entity? */
    isInterested: Boolean,
    /** Number of members/interested parties */
    memberCount: Int,
    /** List of members for the expandable voter list */
    members: List[Person],
    /** Optional extra CSS class for the card */
    extraClass: Option[String] = None,
  )

  /** Create an entity card with the unified TopicCard structure.
    *
    * @param config Card configuration (interest state, members, etc.)
    * @param contentSlot Entity-specific content (title, meta info)
    * @param actionsSlot Entity-specific actions (edit buttons, slack link)
    * @param transition Optional transition for animation
    * @param onSwipeLeft Optional left swipe action (not interested / leave)
    * @param onSwipeRight Optional right swipe action (interested / join)
    * @param leftIcon Icon to show for left swipe (default: "✗")
    * @param rightIcon Icon to show for right swipe (default: "♥")
    */
  def apply(
    config: Config,
    contentSlot: HtmlElement,
    actionsSlot: HtmlElement,
    transition: Option[Transition] = None,
    onSwipeLeft: Option[() => Unit] = None,
    onSwipeRight: Option[() => Unit] = None,
    leftIcon: String = "✗",
    rightIcon: String = "♥",
  ): HtmlElement =
    // Track expanded state for this card's member list
    val expandedKey = s"entity-${config.entityKey}"
    val voterListExpanded = AppState.expandedEntityId.signal.map(_ == Some(expandedKey))

    val cardContent = div(
      cls := "TopicCard",
      cls := (if config.isInterested then "TopicCard--interested" else "TopicCard--neutral"),
      config.extraClass.map(cls := _).getOrElse(emptyMod),
      // Animation support
      transition match
        case Some(value) =>
          val $translateY = value.signal.map {
            case TransitionStatus.Inserting => -100.0
            case TransitionStatus.Removing => 100.0
            case TransitionStatus.Active => 0.0
          }.spring
          Seq(
            value.height,
            transform <-- $translateY.map(pct => s"translateY($pct%)")
          )
        case None => emptyMod
      ,
      // Main row: vote indicator + content + actions
      div(
        cls := "TopicCard-main",
        // Interest indicator (left side)
        div(
          cls := "VoteIndicator",
          cls <-- voterListExpanded.map(if _ then "VoteIndicator--expanded" else ""),
          cls := (if config.isInterested then "VoteIndicator--interested" else ""),
          if config.isInterested || config.memberCount > 0 then cls := "VoteIndicator--visible" else emptyMod,
          if config.memberCount > 0 then cls := "VoteIndicator--clickable" else emptyMod,
          onClick.stopPropagation --> Observer { _ =>
            if config.memberCount > 0 then
              val currentlyExpanded = AppState.expandedEntityId.now()
              if currentlyExpanded == Some(expandedKey) then
                AppState.expandedEntityId.set(None)
              else
                AppState.expandedEntityId.set(Some(expandedKey))
          },
          // Icon based on interest
          span(
            cls := "VoteIndicator-icon",
            if config.isInterested then "♥" else ""
          ),
          // Member count
          if config.memberCount > 0 then
            span(cls := "VoteIndicator-count", config.memberCount.toString)
          else
            span(),
        ),
        // Content (center)
        div(
          cls := "TopicCard-content",
          contentSlot,
        ),
        // Actions (right)
        div(
          cls := "TopicCard-actions",
          actionsSlot,
        ),
      ),
      // Expandable member list (below main row)
      ExpandableVoterList(
        config.members,
        voterListExpanded,
        AppState.approvedUsers.signal,
      ),
    )

    // Wrap in swipeable card if swipe actions are provided
    val hasSwipeActions = onSwipeLeft.isDefined || onSwipeRight.isDefined

    if hasSwipeActions then
      SwipeableCard[SwipeDirection](
        cardContent = cardContent,
        onAction = Observer {
          case SwipeDirection.Left => onSwipeLeft.foreach(_())
          case SwipeDirection.Right => onSwipeRight.foreach(_())
        },
        leftAction = onSwipeLeft.map(_ => SwipeableCard.Action(SwipeDirection.Left, leftIcon)),
        rightAction = onSwipeRight.map(_ => SwipeableCard.Action(SwipeDirection.Right, rightIcon)),
      )
    else
      cardContent
