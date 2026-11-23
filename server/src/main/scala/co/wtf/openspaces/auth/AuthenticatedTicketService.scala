package co.wtf.openspaces.auth

import co.wtf.openspaces.{Person, Ticket}
import zio.*

/** Issues single-use WebSocket tickets, each bound to the user it was issued for. */
case class AuthenticatedTicketService(
  tickets: Ref[Map[Ticket, Person]]):

  def create(owner: Person): UIO[Ticket] =
    for
      ticket <- Random.nextUUID.map(Ticket(_))
      _      <- tickets.update(_ + (ticket -> owner))
    yield ticket

  /** Consume a ticket, returning the user it was issued for. */
  def use(ticket: Ticket): IO[String, Person] =
    tickets
      .modify(existing => (existing.get(ticket), existing - ticket))
      .someOrFail("Ticket is not valid or has already been used.")

object AuthenticatedTicketService:
  val layer: ULayer[AuthenticatedTicketService] =
    ZLayer.fromZIO(Ref.make(Map.empty[Ticket, Person]).map(AuthenticatedTicketService(_)))
