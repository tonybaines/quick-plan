package workouts

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.github.mgifos.workouts._
import com.github.mgifos.workouts.model._
import com.typesafe.scalalogging.Logger

import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContextExecutor, Future}

object Workouts8010 extends App {


  val log = Logger(getClass)

  implicit val system: ActorSystem = ActorSystem("quick-plan")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  val HRZ_1 = Some(HrZoneTarget(1))
  val HRZ_3 = Some(HrZoneTarget(3))
  val HRZ_4 = Some(HrZoneTarget(4))
  val HRZ_5 = Some(HrZoneTarget(5))
  val THRESHOLD_PACE = Some(PaceTarget("4:25".perKm, "4:50".perKm))
  val INTERVALS_PACE = Some(PaceTarget("3:47".perKm, "4:25".perKm))
  val REPEATS_PACE = Some(PaceTarget("3:23".perKm, "3:47".perKm))

  implicit class SmartString(s: String) {
    def perKm = Pace(DistanceUnits.km, s)
  }

  val email = System.getProperty("email")
  val password = System.getProperty("password")
  implicit val garmin: GarminConnect = new GarminConnect(email, password)

  implicit val config = Config(delete = true, mode = Some(Modes.`import`))

  val stdWarmup = Seq(WarmupStep(LapButtonPressed))
  val stdCooldown = Seq(CooldownStep(LapButtonPressed))

  def run(name: String, steps: Seq[Step]): WorkoutDef =
    WorkoutDef("running", name, stdWarmup ++ steps ++ stdCooldown)

  def run(name: String, steps: Step): WorkoutDef = run(name, Seq(steps))

  def intervals(reps: Int, effort: Step, recovery: Step) = RepeatStep(reps, Seq(effort, recovery))

  def easyRun(mins: Double) = zoneRun(HRZ_1, mins)

  def z3Run(mins: Int) = zoneRun(HRZ_3, mins)

  def z4Run(mins: Int) = zoneRun(HRZ_4, mins)

  def z5Run(mins: Double) = zoneRun(HRZ_5, mins)

  def recover(duration: TimeDuration) = RecoverStep(duration)

  def hard(duration: TimeDuration) = IntervalStep(duration)

  def intervalPace(distance: DistanceDuration) = IntervalStep(distance, INTERVALS_PACE)

  def repeatsPace(time: TimeDuration) = IntervalStep(time, REPEATS_PACE)

  def thresholdPace(time: TimeDuration) = IntervalStep(time, THRESHOLD_PACE)

  def thresholdPace(distance: DistanceDuration) = IntervalStep(distance, THRESHOLD_PACE)

  def thresholdPace() = IntervalStep(LapButtonPressed, THRESHOLD_PACE)

  def longRun(d: Double, units: DistanceUnits.DistanceUnit = DistanceUnits.km) =
    IntervalStep(DistanceDuration(d.toFloat, units), HRZ_1)

  def fastFinish(steadyMins: Int, fastMins: Int) = Seq(
    easyRun(steadyMins), z3Run(fastMins)
  )

  def zoneRun(hrZoneTarget: Some[HrZoneTarget], mins: Double): Step = {
    val minutes = Math.floor(mins).toInt
    val seconds = ((mins - minutes) * 60).toInt
    IntervalStep(TimeDuration(minutes = minutes, seconds = seconds), hrZoneTarget)
  }

  def toKm(miles: Double) = (miles * 1.609344).toInt

  implicit class SmartInt(n: Int) {
    def secs = TimeDuration(seconds = n)

    def km = DistanceDuration(n, DistanceUnits.km)
  }

  implicit class SmartDouble(n: Double) {
    def km = DistanceDuration(n.floatValue(), DistanceUnits.km)
  }

  val workouts: Seq[WorkoutDef] = Seq(
    run("1km Intervals", intervals(6, intervalPace(1 km), recover(120 secs))),
    run("2km Intervals", intervals(3, intervalPace(2 km), recover(180 secs))),
    run("3km Intervals", intervals(3, intervalPace(3 km), recover(240 secs))),
    run("Threshold Repeats", intervals(4, thresholdPace(2 km), recover(120 secs))),
    run("Threshold Run", thresholdPace()),
    run("Short Intervals", intervals(8, repeatsPace(60 secs), recover(120 secs))),
    run("Hill Repeats", intervals(12, hard(30 secs), recover(90 secs))),
    run(name = "Pyramid 1:2:3:4:5:5:4:3:2:1", Seq(
      IntervalStep(60 secs), RecoverStep(30 secs),
      IntervalStep(120 secs), RecoverStep(60 secs),
      IntervalStep(180 secs), RecoverStep(90 secs),
      IntervalStep(240 secs), RecoverStep(120 secs),
      IntervalStep(300 secs), RecoverStep(150 secs),
      IntervalStep(300 secs), RecoverStep(150 secs),
      IntervalStep(240 secs), RecoverStep(120 secs),
      IntervalStep(180 secs), RecoverStep(90 secs),
      IntervalStep(120 secs), RecoverStep(60 secs),
      IntervalStep(60 secs)
    )),
    WorkoutDef("running", "10k Progression", Seq(
      IntervalStep(2 km, Some(PaceTarget("6:00".perKm, "5:50".perKm))),
      IntervalStep(2 km, Some(PaceTarget("4:50".perKm, "4:40".perKm))),
      IntervalStep(2 km, Some(PaceTarget("4:40".perKm, "4:35".perKm))),
      IntervalStep(2 km, THRESHOLD_PACE),
      IntervalStep(1 km, Some(PaceTarget("4:20".perKm, "4:30".perKm))),
      IntervalStep(1 km, Some(PaceTarget("4:10".perKm, "4:20".perKm)))
    ) ++ stdCooldown),
    WorkoutDef("running", "Stryd CPT (time)", Seq(
      RecoverStep(600 secs),
      IntervalStep(540 secs),
      RecoverStep(1800 secs),
      IntervalStep(180 secs)
    ) ++ stdCooldown),
    WorkoutDef("running", "Stryd CPT (distance)", Seq(
      RecoverStep(600 secs),
      IntervalStep(2.4 km),
      RecoverStep(1800 secs),
      IntervalStep(1.2 km)
    ) ++ stdCooldown),
  )
  val workouts_8020: Seq[WorkoutDef] = Seq(
    // Speed Play
    run("SP1", intervals(3, z4Run(2), easyRun(2))),
    run("SP2", intervals(5, z5Run(1), easyRun(2))),
    run("SP3", intervals(4, z4Run(2), easyRun(2))),
    run("SP4", intervals(6, z5Run(1), easyRun(2))),
    run("SP5", intervals(5, z4Run(2), easyRun(2))),
    run("SP6", intervals(7, z5Run(1), easyRun(2))),
    run("SP7", intervals(6, z4Run(2), easyRun(2))),
    run("SP8", intervals(8, z5Run(1), easyRun(2))),
    // Short Intervals
    run("SI1", intervals(6, z5Run(1), easyRun(2))),
    run("SI2", intervals(8, z5Run(1), easyRun(2))),
    run("SI3", intervals(6, z5Run(1.5), easyRun(2.5))),
    run("SI4", intervals(10, z5Run(1), easyRun(2))),
    run("SI5", intervals(8, z5Run(1.5), easyRun(2.5))),
    run("SI6", intervals(12, z5Run(1), easyRun(2))),
    run("SI7", intervals(10, z5Run(1.5), easyRun(2.5))),
    run("SI8", intervals(12, z5Run(1.5), easyRun(2.5))),
    // Hill Repeats
    run("HR1", intervals(6, z5Run(0.5), easyRun(1.5))),
    run("HR2", intervals(8, z5Run(0.5), easyRun(1.5))),
    run("HR3", intervals(6, z5Run(1.5), easyRun(2))),
    run("HR4", intervals(10, z5Run(0.5), easyRun(1.5))),
    run("HR5", intervals(12, z5Run(0.5), easyRun(1.5))),
    run("HR6", intervals(8, z5Run(1), easyRun(2))),
    run("HR7", intervals(6, z5Run(1.5), easyRun(2.5))),
    run("HR8", intervals(10, z5Run(1), easyRun(2))),
    // Long Intervals
    run("LI1", intervals(3, z4Run(3), easyRun(2))),
    run("LI2", intervals(4, z4Run(3), easyRun(2))),
    run("LI3", intervals(3, z4Run(5), easyRun(3))),
    run("LI4", intervals(5, z4Run(3), easyRun(2))),
    run("LI5", intervals(6, z4Run(3), easyRun(2))),
    run("LI6", intervals(4, z4Run(5), easyRun(3))),
    run("LI7", intervals(5, z4Run(5), easyRun(3))),
    run("LI8", intervals(6, z4Run(5), easyRun(3))),
    // Cruise Intervals
    run("CI1", intervals(4, z3Run(5), easyRun(3))),
    run("CI2", intervals(4, z3Run(8), easyRun(3))),
    run("CI3", intervals(4, z3Run(10), easyRun(3))),
    run("CI4", intervals(4, z3Run(12), easyRun(3))),
    run("CI5", intervals(4, z3Run(15), easyRun(3))),
    // Long Run
    run("LR1", longRun(toKm(4.5))),
    run("LR2", longRun(toKm(5.5))),
    run("LR3", longRun(toKm(6.5))),
    run("LR4", longRun(toKm(7.5))),
    run("LR5", longRun(toKm(8.5))),
    run("LR6", longRun(toKm(9.5))),
    run("LR7", longRun(toKm(10.5))),
    // Fast Finish
    run("FF2", fastFinish(20, 5)),
    run("FF3", fastFinish(20, 10)),
    run("FF4", fastFinish(25, 10)),
    run("FF5", fastFinish(25, 12)),
    run("FF6", fastFinish(30, 12)),
    run("FF7", fastFinish(35, 12)),
    run("FF8", fastFinish(35, 15)),
    run("FF9", fastFinish(40, 15)),
    run("FF10", fastFinish(45, 15)),
    // Tempo Run
    run("TR1", z3Run(15)),
    run("TR2", z3Run(18)),
    run("TR3", z3Run(20)),
    run("TR4", z3Run(24)),
    run("TR5", z3Run(28)),
    run("TR6", z3Run(30)),
    run("TR7", z3Run(32)),
    run("TR8", z3Run(36)),
    run("TR9", z3Run(40)),
    run("TR10", z3Run(45)),
    // Mixed Intervals
    run("MI1", Seq(
      z5Run(1),
      easyRun(2),
      z4Run(3),
      easyRun(2),
      z3Run(5),
      easyRun(2),
      z4Run(3),
      easyRun(2),
      z5Run(1),
    ))
  )

  val cleanUp: Seq[String] = Seq(
    // any named workouts to delete (e.g. after rename)
    "Hill HIIT (60/120)", "Hill HIIT (30/90)", "Short Intervals (60/120)"

  )

  garmin.login().flatMap {
    case Right(s) =>
      implicit val session: GarminSession = s
      for {
        maybeDeleteMessage <- deleteWorkoutsTask(cleanUp ++ workouts.map(_.name))
        garminWorkouts <- createWorkoutsTask(workouts)
      } yield {
        log.info("\nStatistics:")
        maybeDeleteMessage.foreach(msg => log.info("  " + msg))
        log.info(s"  ${garminWorkouts.size} imported")
      }
    case Left(loginFailureMessage) =>
      log.error(loginFailureMessage)
      Future.successful(())
  }.onComplete({
    _ -> system.terminate()
  })

  /**
    * Deletes existing workouts with the same names or not
    */
  def deleteWorkoutsTask(workouts: Seq[String])(implicit config: Config, garmin: GarminConnect, s: GarminSession):
  Future[Option[String]] = {
    if (config.delete)
      garmin.deleteWorkouts(workouts).map(c => Some(s"$c deleted"))
    else
      Future.successful(None)
  }


  def createWorkoutsTask(workouts: Seq[WorkoutDef])(implicit config: Config, garmin: GarminConnect, session: GarminSession): Future[Option[Seq[GarminWorkout]]] = {
    if (config.mode.exists(Seq(Modes.`import`, Modes.schedule).contains))
      garmin.createWorkouts(workouts).map(Option.apply)
    else
      Future.successful(None)
  }

}
