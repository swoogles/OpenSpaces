package co.wtf.openspaces

import java.util.UUID
import zio.*
import zio.direct.*
import zio.json.JsonCodec

case class AuthenticatedTicketService(tickets: Ref[List[Ticket]]):
  val create: ZIO[Any, Nothing, Ticket] =
    defer:
      val newTicket = Ticket(Random.nextUUID.debug("Next random uuid").run)
      tickets.update(_ :+ newTicket).run
      newTicket

  def use(ticket: Ticket): ZIO[Any, String, Unit] =
    defer:
      val validTicket = tickets.getAndUpdate { existingTickets =>
        if (existingTickets.contains(ticket)) {
          existingTickets.filterNot(_ == ticket)
        } else {
          existingTickets
        }
      }.map(_.contains(ticket)).run
      if(validTicket)
        ZIO.unit.run
      else
        ZIO.fail("Ticket is not valid or has already been used.").run

object AuthenticatedTicketService:
  val layer =
    ZLayer.fromZIO:
      defer:
        val ticketsRef = Ref.make[List[Ticket]](List.empty).run
        AuthenticatedTicketService(ticketsRef)