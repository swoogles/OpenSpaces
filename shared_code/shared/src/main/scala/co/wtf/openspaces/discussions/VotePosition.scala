package co.wtf.openspaces.discussions

import zio.json.*

enum VotePosition derives JsonCodec:
  case Interested, NotInterested