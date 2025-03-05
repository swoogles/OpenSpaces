import zio.json.JsonCodec
import zio.json.*

case class Discussion(topic: String) derives JsonCodec

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