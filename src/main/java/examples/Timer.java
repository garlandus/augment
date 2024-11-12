package examples;

import static augmented.augmentJ.*;
import static augmented.augmentJ.sequence;
import static util.AugmentUtil.*;
import static util.ZioUtil.sequence;
import util.AugmentUtil.Unit;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;
import static java.util.stream.IntStream.range;
import javax.sound.sampled.*;
import scala.runtime.*;

import zio.ZIO;

/** Adapted from https://www.learnscala.dev/challenge-page/zio-2-functional-programming-fundamentals-course */

public class Timer
{
  public enum FailureReason {
    NoCommandLineArgs, InvalidNumberOfArgs, InvalidIntForMinutes, InvalidIntForGain, CouldNotCreateAudioStream,
    CouldNotCreateClip, CouldNotPlayClip
  }

  public static ZIO<Object, Object, Unit> getTimerIO(List<String> options) {
    return getTimerIO(options, "src/main/resources/cow.wav");
  }

  public static ZIO<Object, Object, Unit> getTimerIO(List<String> options, String soundFile) {
    var argList = zioSucceed(options);

    var prog = sequence(
      ()                    -> argList,
      args                  -> checkArgCount(args),
      (args, _)             -> getMinutesToWait(args),
      (args, _, _)          -> getGainControl(args),
      (_, minutesToWait, _) -> printTimerStartingMsg(minutesToWait),
      (minutesToWait, _, _) -> countdownEffect(minutesToWait),
      (gainControl, _, _)   -> playSoundFileEffect(soundFile, gainControl)
    );

    return prog.io();
  }

  public static ZIO<Object, FailureReason, BoxedUnit> checkArgCount(List<String> args) {
    return zioFailIf(() -> (args.size() == 1 || args.size() == 2), FailureReason.InvalidNumberOfArgs);
  }

  public static ZIO<Object, FailureReason, Integer> getMinutesToWait(List<String> args) {
    return zioAttempt(() -> Integer.parseInt(args.get(0)), FailureReason.InvalidIntForMinutes);
  }

  public static ZIO<Object, FailureReason, Integer> getGainControl(List<String> args) {
    var DEFAULT_GAIN = -39;
    return args.size() == 2 ? zioAttempt(() -> Integer.parseInt(args.get(1)), FailureReason.InvalidIntForGain)
        : augmented.augmentJ.<FailureReason, Integer>zioSucceed(DEFAULT_GAIN);
  }

  public static ZIO<Object, Object, Unit> printTimerStartingMsg(Integer minutesToWait) {
    return zioSucceed(printlnUnit("Timer started. Wait time is " + minutesToWait + " minutes..."));
  }

  public static ZIO<Object, Object, BoxedUnit> countdownEffect(Integer minutesToWait) {
    List<Integer> l = range(1, minutesToWait + 1).boxed().collect(Collectors.toList());
    return zioForeachDiscard(
      l, 
      minute -> sequence(
                  ()  -> zioSleep(1),
                  _   -> zioPrintLine("time remaining: " + (minutesToWait - minute))
      ).io()
    );
  }

  /** 
    * This sequence can be more or less copy/pasted to Scala (the arrows need to be changed from -> to =>)
    */
  public static ZIO<Object, Object, Unit> playSoundFileEffect(String filePath, Integer gainControl) {
    var prog =
      sequence(
        ()                        -> println("----- PLAYING SOUND FILE NOW [" + filePath + "] ---"),
        _                         -> new File(filePath),
        (_, file)                 -> AudioSystem.getAudioInputStream(file),
        (_, _)                    -> AudioSystem.getClip(),
        (audioInputStream, clip)  -> clip.open(audioInputStream),
        (clip, _)                 -> clip.start()
      );

    return prog.io();
  }
}
