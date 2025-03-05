package co.wtf.openspaces

import co.wtf.openspaces.{Discussion, Room, ScheduleSlot}
import com.raquo.laminar.api.L.{*, given}
import io.laminext.websocket.WebSocket
import org.scalajs.dom
import zio.json.*
import org.scalajs.dom.window

val localStorage = window.localStorage

private def getOrCreatePersistedName(): Var[String] =
  val name =
    try {
      val retrieved =
        localStorage
          .getItem("name")
      Option.when(retrieved != null && !retrieved.isBlank)(
        retrieved
      )
    }
    catch {
      case e: Exception =>
        println("Error retrieving existing name: " + e)
        None
    }
  Var(name.getOrElse(""))

private def BannerLogo() =
  div( width := "100%",
    img(cls := "LogoImg", src := "./wtf-web-nodate.jpg", role := "img")
  )

private def NameBadge(textVar: Var[String]) =
  div(cls := "Banner",
    img(cls := "LogoImg", src := "./wtf-web-nodate.jpg", role := "img"), div(
        input(
          placeholder := "Enter your name",
          value <-- textVar,
          onInput.mapToValue --> textVar,
          textVar --> Observer {
            (value: String) =>
              localStorage.setItem("name", value)
              println("Name: " + value)
          }
        ),
      )
  )

private def TopicSubmission(submitEffect: Observer[Discussion], name: StrictSignal[String]) =

  val intBus = new EventBus[Int]
  val textVar = Var("")
  div( cls := "Flex",
    span(
      textArea(
        fontFamily := "Roboto", placeholder := "Create a topic...", onClick.mapTo(1) --> intBus,
        value <-- textVar,
        onInput.mapToValue --> textVar,
        onMouseOver --> { ev => println(ev) },
        onChange --> { _ => println("committed") },
        onBlur.mapTo("blur") --> Observer {blurValue => println(blurValue)}
      )
    ),
    button(
      onClick.mapTo(textVar.now()).map(topicTitle => Discussion.apply(topicTitle, name.now(), Set(name.now()))) --> submitEffect,
      "Submit"
    )
  )


private def DiscussionsToReview(topics: Signal[List[Discussion]], name: StrictSignal[String]) =
  val topicUpdates = WebSocket.url("/discussions").string.build()
  div(
    cls := "TopicsContainer", topicUpdates.connect,
    children <-- topics.map {
      topics =>
        topics.sortBy(_.votes).sortWith(_.votes > _.votes).map {
          topic =>
            div( cls := "TopicCard",
              div( cls := "TopicBody",
                div(
                  display := "flex",
                  justifyContent := "space-between",
                  h3(topic.topic),
                  if (List("bill", "emma").exists( admin =>  name.now().toLowerCase().contains(admin)))
                  button(
                    cls:="delete-topic",
                    color := "red",
                    border := "none", backgroundColor := "transparent", onClick --> Observer {
                      _ =>
                        topicUpdates.sendOne(DiscussionAction.Delete(topic.topic).asInstanceOf[DiscussionAction].toJson)
                    },
                    "x"
                  )
                  else span()
                ), span(cls := "VoteContainer",p(topic.facilitator), p("Votes ", topic.votes, " "),

                if topic.interestedParties.contains(name.now()) then
                  button(
                    cls := "RemoveButton", onClick --> Observer {
                      _ =>
                        topicUpdates.sendOne(DiscussionAction.RemoveVote(topic.topic, name.now()).asInstanceOf[DiscussionAction].toJson)
                    },
                    "-"
//                    img(src := "./minus-icon.svg", role := "img") // TODO can we get a minus icon?
                  )
                else
                button(
                  cls := "AddButton", onClick --> Observer {
                    _ =>
                      topicUpdates.sendOne(DiscussionAction.Vote(topic.topic, name.now()).asInstanceOf[DiscussionAction].toJson)
                  },
                  img(src := "./plus-icon.svg", role := "img")
                ),
              ))
            )
        }
    }
  )


enum AppView:
  case Home
  case ScheduleView
  case SubmitTopic

enum Room:
  case King
  case ArtGallery
  case Hawk
  case DanceHall

case class ScheduleSlot(room: Room)

def DaySchedule(slots: Var[List[ScheduleSlot]]) =
  div(
    children <-- slots.signal.map {
      slots =>
        slots.map {
          slot =>
            div(
              slot.toString
            )
        }
    }
  )

object FrontEnd extends App:
    lazy val container = dom.document.getElementById("app")
    import io.laminext.websocket.*

    val topicUpdates = WebSocket.url("/discussions").string.build()

    val topicsToReview: Var[List[Discussion]] =
      Var(List.empty)

    val error: Var[Option[String]] =
      Var(None)

    val submitNewTopic: Observer[Discussion] = Observer {
      discussion =>
        if (discussion.facilitator.trim.length < 2)
          error.set(Some("User name too short. Tell us who you are!"))
        else if (discussion.topic.trim.length < 10)
          error.set(Some("Topic too short. More details please."))
        else
          error.set(None)
          topicUpdates.sendOne(DiscussionAction.Add(discussion).asInstanceOf[DiscussionAction].toJson) // TODO Json
    }

    val app = {
      val name = getOrCreatePersistedName()
      div(
        cls := "PageContainer",
        topicUpdates.connect,
        topicUpdates.received --> Observer {
          (event: String) =>
            println("From MY WS: " + event)
            topicsToReview.update(existing =>
              event.fromJson[DiscussionAction] match
                case Left(value) =>
                  println("Uh oh, bad discussion sent from server: " + value)
                  existing
                case Right(value) =>
                  value match
                    case DiscussionAction.Delete(topic) =>
                      println("Should delete: " + topic)
                      val initialSize =  existing.length
                      println("initial size: " + initialSize)
                      val res = existing.filterNot(_.topic == topic)
                      println("new size: " + res.length)
                      res
                    case DiscussionAction.Add(discussion) =>
                      println("Should add: " + discussion)
                      if (existing.exists(_.topic == discussion.topic))
                        existing.map {
                          existingDiscussion =>
                            if existingDiscussion.topic == discussion.topic then
                              discussion
                            else
                              existingDiscussion
                        }
                      else
                        discussion :: existing
                    case DiscussionAction.Vote(voteTopic, voter) =>
                      existing.map {
                        discussion =>
                          if (discussion.topic == voteTopic)
                            println("Bumping the count")
                            discussion.copy(interestedParties = discussion.interestedParties + voter)
                          else
                            discussion
                      }
                    case DiscussionAction.RemoveVote(voteTopic, voter) =>
                      existing.map {
                        discussion =>
                          if (discussion.topic == voteTopic)
                            println("Removing the count")
                            discussion.copy(interestedParties = discussion.interestedParties - voter)
                          else
                            discussion
                      }
            )
        },
        child <-- error.signal.map {
          case Some(value) =>
            div(
              cls := "Error",
              color := "red",
              value
            )
          case None =>
            div()
        },
        NameBadge(name),
        TopicSubmission(submitNewTopic, name.signal),
        DiscussionsToReview(topicsToReview.signal, name.signal),
      )
    }
    render(container, app)