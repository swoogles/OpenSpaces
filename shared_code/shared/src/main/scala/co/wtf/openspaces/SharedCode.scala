package co.wtf.openspaces

import zio.json.*

case class Discussion(topic: String, votes: Int) derives JsonCodec

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