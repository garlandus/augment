package examples

import augmented._
import augmented.given
import augmented.Extensions._
import mappable._
import mappable.given
import mappablethirdparty.given

import zio._
import zio.Console._
import javax.sound.sampled._
import java.io.{File, IOException}

/** Adapted from https://www.learnscala.dev/challenge-page/zio-2-functional-programming-fundamentals-course */

enum FailureReason:
  case  NoCommandLineArgs, InvalidNumberOfArgs, InvalidIntForMinutes, InvalidIntForGain,
        CouldNotCreateAudioStream, CouldNotCreateClip, CouldNotPlayClip

import FailureReason._

def getTimerIO(options: Seq[String])
              (useComprehensions: Boolean = false, usePlainValues: Boolean = true,
               soundFilePath: String = "src/main/resources/cow.wav") =

  val argList = ZIO.succeed(options.toList)
  val DefaultGain = -30

  val usageMessage = """
      | Usage:  timer minutes-before-alarm <gain-control>
      |         gain-control should be something like -10 or -20
  """.stripMargin.trim

  val invalidIntForMinutesParameterMsg = "Invalid int for the MINUTES parameter"
  val invalidIntForGainParameterMsg = "Invalid int for the GAIN parameter"
  val invalidNumberOfArgsMsg = "Invalid number of command-line parameters"
  val couldNotCreateAudioStreamMsg = "Could not create an AudioInputStream"
  val couldNotCreateClipMsg = "Could not create an audio clip"
  val couldNotPlayClipMsg = "Could not play the audio clip"

  def checkForZeroArgs(args: Seq[String]): ZIO[Any, FailureReason, Unit] =
    if args.size == 0 then ZIO.fail(NoCommandLineArgs) else ZIO.succeed(())

  def checkThatWeHaveTwoArgs(args: Seq[String]): ZIO[Any, FailureReason, Unit] =
    if args.size == 1 || args.size == 2 then ZIO.succeed(()) else ZIO.fail(InvalidNumberOfArgs)

  def checkArgCount(args: Seq[String]): ZIO[Any, FailureReason, Unit] =
    if args.size == 1 || args.size == 2 then ZIO.succeed(()) else ZIO.fail(InvalidNumberOfArgs)

  def getMinutesToWait(args: Seq[String]): ZIO[Any, FailureReason, Int] =
    ZIO
      .attempt(args.head.toInt)
      .orElseFail(InvalidIntForMinutes)

  def getGainControl(args: Seq[String]): ZIO[Any, FailureReason, Int] =
    if args.size == 2 then
      ZIO
        .attempt(args(1).toInt)
        .orElseFail(InvalidIntForGain)
    else ZIO.succeed(DefaultGain)

  def printTimerStartingMsgZ(minutesToWait: Int) =
    ZIO.succeed(println(s"Timer started. Wait time is $minutesToWait minutes."))

  def printTimerStartingMsg(minutesToWait: Int) =
    println(s"Timer started. Wait time is $minutesToWait minutes.")

  def countdownEffect(minutesToWait: Int): ZIO[Any, Nothing, Unit] =
    ZIO.foreachDiscard(1 to minutesToWait): minute =>
      ZIO.sleep(1.second) *>
        printLine(s"time remaining: ${minutesToWait - minute}").orDie

  /** 
    * The main sequence in "traditional" for-comprehension style
    */
  def playSoundFileEffectOld(path: String, gainControl: Int): ZIO[Any, Throwable, Unit] =
    for
      _                         <- printLine("----- PLAYING SOUND FILE NOW [" + path + "] (comprehensions) ---").orDie
      file                      <- ZIO.succeed(File(soundFilePath))
      audioInputStream          <- ZIO.attempt(AudioSystem.getAudioInputStream(file))
      clip                      <- ZIO.attempt(AudioSystem.getClip)
      _                         <- ZIO.attempt(clip.open(audioInputStream))
      _                         <- ZIO.attempt(clip.start())
    yield ()

  /** 
    * This sequence can be more or less copy/pasted to Java (the arrows need to be changed from => to ->)
    * The right-hand side values are plain instead of lifted (which is not an option in the for-comprehension)
    */
  def playSoundFileEffectPlain(path: String, gainControl: Int): ZIO[Any, Exception, Unit] =
    
    sequence(
      println("----- PLAYING SOUND FILE NOW [" + path + "] (plain values) ---"),
      _                         => new File(path),
      (_, file)                 => AudioSystem.getAudioInputStream(file),
      (_, _)                    => AudioSystem.getClip(),
      (audioInputStream, clip)  => clip.open(audioInputStream),
      (clip, _)                 => clip.start()
    )

  /** 
    * You can also use lifted values
    */
  def playSoundFileEffectLifted(path: String, gainControl: Int): ZIO[Any, Throwable, Unit] =

    sequence(
      printLine("----- PLAYING SOUND FILE NOW [" + path + "] (lifted values) ---").orDie,
      _                         => ZIO.succeed(new File(soundFilePath)),
      (_, file)                 => ZIO.attempt(AudioSystem.getAudioInputStream(file)),
      (_, _)                    => ZIO.attempt(AudioSystem.getClip),
      (audioInputStream, clip)  => ZIO.attempt(clip.open(audioInputStream)),
      (clip, _)                 => ZIO.attempt(clip.start())
    )

  val playSoundFE =
    if useComprehensions then playSoundFileEffectOld else
      if usePlainValues then playSoundFileEffectPlain else playSoundFileEffectLifted
    
  def playSound(options: Seq[String]) =

    val argList = ZIO.succeed(options.toList)

    val io =
      sequence(
        argList,
        checkArgCount,
        (args, _)             => getMinutesToWait(args),
        (args, _, _)          => getGainControl(args),
        (_, minutesToWait, _) => printTimerStartingMsg(minutesToWait),
        (minutesToWait, _, _) => countdownEffect(minutesToWait),
        (gainControl, _, _)   => playSoundFE(soundFilePath, gainControl).orElseFail(CouldNotPlayClip)
      )

    io.foldZIO(
      failure =>
        failure match
          case NoCommandLineArgs         => ZIO.fail(usageMessage)
          case InvalidNumberOfArgs       => ZIO.fail(invalidNumberOfArgsMsg)
          case InvalidIntForMinutes      => ZIO.fail(invalidIntForMinutesParameterMsg)
          case InvalidIntForGain         => ZIO.fail(invalidIntForGainParameterMsg)
          case CouldNotCreateAudioStream => ZIO.fail(couldNotCreateAudioStreamMsg)
          case CouldNotCreateClip        => ZIO.fail(couldNotCreateClipMsg)
          case CouldNotPlayClip          => ZIO.fail(couldNotPlayClipMsg)
      ,
      success => ZIO.succeed(())
    )

  playSound(options)
