package co.wtf.openspaces.services

import org.scalajs.dom

import co.wtf.openspaces.{AppState, TopicId, VotePosition}

object AudioService:
  // Shared AudioContext - created lazily on first user interaction
  private var sharedAudioContext: Option[dom.AudioContext] = None

  /** Get or create the shared AudioContext, resuming if suspended */
  private def getAudioContext(): Option[dom.AudioContext] =
    sharedAudioContext match
      case Some(ctx) =>
        // Resume if suspended (browser policy)
        if ctx.state == "suspended" then
          ctx.resume()
        Some(ctx)
      case None =>
        try
          val ctx = new dom.AudioContext()
          sharedAudioContext = Some(ctx)
          Some(ctx)
        catch
          case _: Throwable => None

  /** Initialize audio on first user gesture (call from any click/touch handler) */
  def initAudioOnGesture(): Unit =
    if sharedAudioContext.isEmpty then
      getAudioContext()
    else
      sharedAudioContext.foreach { ctx =>
        if ctx.state == "suspended" then ctx.resume()
      }

  /** Play a satisfying pop/click sound using Web Audio API */
  def playVoteSound(position: VotePosition): Unit =
    if !AppState.soundMuted.now() then
      getAudioContext().foreach { audioContext =>
        try
          val oscillator = audioContext.createOscillator()
          val gainNode = audioContext.createGain()

          // Different sounds for interested vs not interested
          val (startFreq, endFreq) = position match
            case VotePosition.Interested => (600.0, 800.0)  // Rising pop - happy
            case VotePosition.NotInterested => (400.0, 300.0) // Falling thud - muted

          oscillator.`type` = "sine"
          oscillator.frequency.setValueAtTime(startFreq, audioContext.currentTime)
          oscillator.frequency.exponentialRampToValueAtTime(endFreq, audioContext.currentTime + 0.08)

          gainNode.gain.setValueAtTime(0.15, audioContext.currentTime)
          gainNode.gain.exponentialRampToValueAtTime(0.01, audioContext.currentTime + 0.12)

          oscillator.connect(gainNode)
          gainNode.connect(audioContext.destination)

          oscillator.start(audioContext.currentTime)
          oscillator.stop(audioContext.currentTime + 0.12)
        catch
          case _: Throwable => () // Silently fail if audio not available
      }

  /** Trigger celebration for a topic (animation + sound) */
  def celebrateVote(topicId: TopicId, position: VotePosition): Unit =
    // Add to celebrating set
    AppState.celebratingTopics.update(_ + (topicId -> position))

    // Play sound
    playVoteSound(position)

    // Remove from celebrating after animation completes (400ms)
    dom.window.setTimeout(() => {
      AppState.celebratingTopics.update(_ - topicId)
    }, 450)
