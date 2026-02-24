package co.wtf.openspaces

import zio.json.*

case class Room(
  id: Int,
  name: String,
  capacity: Int)
    derives JsonCodec

object Room:
  // These match the seed data in V16__seed_rooms_and_slots.sql
  // Used for tests and examples; runtime values come from database
  val king = Room(0, "King", 30)
  val artGallery = Room(1, "Art Gallery", 20)
  val hawk = Room(2, "Hawk", 15)
  val danceHall = Room(3, "Dance Hall", 10)
  
  val all: List[Room] = List(king, artGallery, hawk, danceHall)
