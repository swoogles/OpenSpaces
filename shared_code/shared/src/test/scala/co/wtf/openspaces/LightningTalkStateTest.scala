package co.wtf.openspaces

import zio.test.*

object LightningTalkStateTest extends ZIOSpecDefault:

  private def makeProposal(
    id: Long,
    speaker: String,
    createdAt: Long,
    assignment: Option[LightningAssignment] = None,
  ): LightningTalkProposal =
    LightningTalkProposal(
      LightningTalkId(id),
      Person(speaker),
      None,
      assignment,
      createdAt,
    )

  override def spec =
    suite("LightningTalkStateTest")(
      test("enforces single proposal lookup per speaker") {
        val proposal1 = makeProposal(1L, "alice", 1000L)
        val proposal2 = makeProposal(2L, "bob", 2000L)
        val state = LightningTalkState(Map(proposal1.id -> proposal1, proposal2.id -> proposal2))
        assertTrue(
          state.speakerHasProposal(Person("alice")),
          !state.speakerHasProposal(Person("charlie")),
          state.proposalForSpeaker(Person("bob")).contains(proposal2),
        )
      },
      test("chooses next open assignment in Tuesday -> Thursday order") {
        val tuesdaySlots = (1 to 10).toList.map(slot =>
          LightningAssignment(
            LightningTalkNight.Tuesday,
            LightningTalkSlot.make(slot).toOption.get,
          ),
        )
        val tuesdayProposals = tuesdaySlots.zipWithIndex.map { case (assignment, index) =>
          val proposal = makeProposal(index.toLong + 1L, s"user$index", index.toLong, Some(assignment))
          proposal.id -> proposal
        }.toMap
        val state = LightningTalkState(tuesdayProposals)

        assertTrue(
          state.nextNightWithOpenSlot.contains(LightningTalkNight.Thursday),
          state.nextOpenAssignment.contains(
            LightningAssignment(LightningTalkNight.Thursday, LightningTalkSlot(1)),
          ),
        )
      },
      test("draw result applies exact assignment mapping") {
        val proposal1 = makeProposal(1L, "alice", 1000L)
        val proposal2 = makeProposal(2L, "bob", 2000L)
        val state = LightningTalkState(Map(proposal1.id -> proposal1, proposal2.id -> proposal2))
        val drawResult = LightningTalkActionConfirmed.DrawForNightResult(
          LightningTalkNight.Tuesday,
          List(
            LightningDrawAssignment(
              proposal1.id,
              LightningAssignment(LightningTalkNight.Tuesday, LightningTalkSlot(1)),
            ),
          ),
        )
        val updated = state(drawResult)

        assertTrue(
          updated.proposals(proposal1.id).assignment.contains(
            LightningAssignment(LightningTalkNight.Tuesday, LightningTalkSlot(1)),
          ),
          updated.proposals(proposal2.id).assignment.isEmpty,
        )
      },
      test("drawForNight rejects when requested night is not next open night") {
        // With an empty state, Tuesday is next, so requesting Thursday should fail
        val state = LightningTalkState(Map.empty)

        val result = LightningTalkDraw.drawForNight(
          state,
          LightningTalkNight.Thursday,
          identity,
        )

        assertTrue(
          result == Left(
            LightningDrawError.NightIsNotNextOpen(
              LightningTalkNight.Thursday,
              LightningTalkNight.Tuesday,
            ),
          ),
        )
      },
      test("drawForNextNight uses no-arg behavior and picks concrete next night") {
        val proposals = List(
          makeProposal(1L, "alice", 1L),
          makeProposal(2L, "bob", 2L),
        ).map(p => p.id -> p).toMap
        val state = LightningTalkState(proposals)

        val result = LightningTalkDraw.drawForNextNight(
          state,
          identity,
        )

        assertTrue(
          result.exists(_.night == LightningTalkNight.Tuesday),
          result.exists(_.assignments.size == 2),
          result.exists(_.assignments.head.assignment == LightningAssignment(LightningTalkNight.Tuesday, LightningTalkSlot(1))),
        )
      },
    )
