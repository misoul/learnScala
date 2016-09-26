package com.interviews

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.util.Sorting.quickSort
import scala.io.Source

object FSM {
  object Signal extends Enumeration {
    type Signal = Value
    val ReviewedEnoughSubmissions, SubmissionGradedFailed, SubmissionGradedPassed = Value
  }
  import Signal._

  sealed trait State { def next(signals: Set[Signal]): Option[State] }
  case object Working extends State {
    def next(signals: Set[Signal]): Option[State] = {
      signals match {
        case set if set.isEmpty => Some(WaitingToReview)
        case _ => throw new IllegalStateException("State " + this + " not expecting signals: " + signals)
      }
    }
  }
  case object WaitingToReview extends State {
    def next(signals: Set[Signal]): Option[State] = {
      signals match {
        case set if set.isEmpty => Some(Reviewing)
        case set if set == Set(SubmissionGradedFailed) => Some(Working)
        case _ => throw new IllegalStateException("State " + this + " not expecting signals: " + signals)
      }
    }
  }
  case object Reviewing extends State {
    def next(signals: Set[Signal]): Option[State] = {
      signals match {
        case set if set==Set(SubmissionGradedFailed) => None
        case set if set==Set(ReviewedEnoughSubmissions) => Some(WaitingForGrade)
        case set if set.isEmpty => Some(WaitingToReview)
        case _ => throw new IllegalStateException("State " + this + " not expecting signals: " + signals)
      }
    }
  }
  case object WaitingForGrade extends State {
    def next(signals: Set[Signal]): Option[State] = {
      signals match {
        case set if set==Set(SubmissionGradedFailed) => Some(Working)
        case set if set==Set(SubmissionGradedPassed) => Some(Finish)
        case _ => throw new IllegalStateException("State " + this + " not expecting signals: " + signals)
      }
    }
  }
  case object Finish extends State {
    def next(signals: Set[Signal]): Option[State] = throw new IllegalStateException("State " + this + " not expecting signals: " + signals)
  }

  val INITIAL_STATE = Working
}

case class Submission(learner: Learner, submitTime: Int, reviews: ArrayBuffer[Review]) extends Ordered[Submission] {
  def update(tick: Int): Boolean = {
    val scoredReviews = reviews.filter(_.score.nonEmpty)
    if (scoredReviews.length == Learner.No_REVIEWS_PER_SUBMISSION) {
      val score = reviews.map(_.score.get).sum
      val signals = if (score < Learner.PASSING_GRADE) Set(FSM.Signal.SubmissionGradedFailed) else Set(FSM.Signal.SubmissionGradedPassed)
      val desiredState = learner.state.next(signals)
      learner.changeState(desiredState, tick)
    } else {
      true
    }
  }

//  def compare(a: Submission, b: Submission): Int = { TODO: REMOVE ME
//    if (a.submitTime != b.submitTime) a.submitTime - b.submitTime else a.learner.id - b.learner.id
//  }

  def compare(b: Submission): Int = if (submitTime != b.submitTime) submitTime - b.submitTime else learner.id - b.learner.id

  override def toString = {
    val scoredReviews = reviews.filter(r => r.score.nonEmpty)
    val gradeTick = if (scoredReviews.length < 3) -1.toString else (reviews(2).createTime + Learner.REVIEW_WINDOW).toString
    val scoreSum = scoredReviews.map(_.score.get).sum.toString
    "Submission:" + learner.id + ":?:" + submitTime + ":" + scoreSum + ":" + gradeTick + ":[" + reviews.mkString(" ") + "]"
    // RequirementL learnerId seqNumber submissionTick ScoreSum gradeTick
  }
  def toStringShort = "S(" + learner.id + ":" + submitTime + ")"
}
object Submission {
  val all = ArrayBuffer.empty[Submission] // See README

  def create(learner: Learner, submitTime: Int, reviews: ArrayBuffer[Review]): Submission = {
    val submission = Submission(learner, submitTime, ArrayBuffer.empty)
    all ++= Seq(submission)
    submission
  }

  def getSubmissionForReview(learner: Learner, tick: Int): Option[Submission] = {
    val submissions = all.filter(s => s.learner.id != learner.id &&
                                !s.reviews.map(_.reviewer.id).contains(learner.id) &&
                                s.reviews.length < 3)
                          .sorted
    if (submissions.length > 0) Some(submissions(0)) else None
  }
}

/*
 - If a review is pending, score==None. Otherwise, score=Int.
 - A scored review might not be processed immediately (e.g. learner might be in Reviewing state and not yet finish).
 */
case class Review(reviewer: Learner, submission: Submission, createTime: Int, var score: Option[Int], var processed: Boolean) {
  def update(tick: Int): Boolean = { // Return true if review is processed; false otherwise
    if (submission.update(tick)) this.processed = true
    this.processed
  }

  override def toString = "Review:" + reviewer.id + ":" + submission.toStringShort + ":" + createTime
}
object Review {
  val all = ArrayBuffer.empty[Review] // See README

  def createPending(reviewer: Learner, submission: Submission, createTime: Int): Review = {
    val review = Review(reviewer, submission, createTime, None, false)
    all ++= Seq(review)
    review
  }

  def unprocessed = all.filter(!_.processed)
}


case class Learner(id: Int, firstStart: Int, firstGrade: Int, reviewBias: Int) {
  import Learner._
  import FSM._
  import FSM.Signal._

  var state: State = INITIAL_STATE //TODO: set private properly for these values
  val submissions = ArrayBuffer.empty[Submission]
  val reviews = ArrayBuffer.empty[Review]
  var pendingReview: Option[Review] = None // used if state == Reviewing

  var start = firstStart
  var grade = firstGrade
  var nextEventTime = firstStart // the next tick to an event to happen. Used to optimize if we have many users

  def scoreReview(review: Review): Review = {
    val score = this.reviewBias + review.submission.learner.grade
    score match {
      case s if s < 0 => review.score = Some(0)
      case s if s > MAX_GRADE => review.score = Some(MAX_GRADE)
      case _ => review.score = Some(score)
    }
    review
  }

  def publishReview(tick: Int): Review = {
    if (state != Reviewing || start + REVIEW_WINDOW > tick) throw new IllegalStateException("Illegal: " + tick + ": " + this )
    val review = pendingReview.get
    scoreReview(review)
    pendingReview = None
    reviews ++= (Seq(review))
    val signals = if (reviews.length == submissions.length * No_REVIEWS_PER_SUBMISSION) Set(ReviewedEnoughSubmissions) else Set.empty[Signal]
    changeState(signals, tick)

    review
  }

  def createSubmission(tick: Int): Submission = {
    if (state != Working || start + WORK_WINDOW > tick) throw new IllegalStateException("Illegal: " + tick + ": " + this)

    changeState(state.next(Set.empty), tick)
    Submission.create(this, tick, ArrayBuffer.empty)
  }

  def tryReview(tick: Int): Option[Review] = {
    if (state != WaitingToReview) throw new IllegalStateException("Illegal: " + tick + ": " + this )

    val submission = Submission.getSubmissionForReview(this, tick)

    if (submission.nonEmpty) {
      changeState(state.next(Set.empty), tick)
      pendingReview = Some(Review.createPending(this, submission.get, tick))
      submission.get.reviews ++= pendingReview
      pendingReview
    } else {
      None
    }
  }

  def changeState(nextState: Option[State], tick: Int): Boolean = {
    nextState match {
      case None => return false
      case Some(Working) => if (state != Working) grade = Math.min(grade + GRADE_IMPROVEMENT, MAX_GRADE)
      case _ =>
    }

    state = nextState.get
    start = tick
    true
  }

  def changeState(signals: Set[Signal], tick: Int): Boolean = {
    val nextState = this.state.next(signals)
    changeState(nextState, tick)
  }

  override def toString = "Learner:" + id + ":" + state + ":" + start + ":" + grade
}
object Learner {
  import FSM._

  val WORK_WINDOW = 50
  val REVIEW_WINDOW = 20
  val GRADE_IMPROVEMENT = 15
  val MAX_GRADE = 100
  val No_REVIEWS_PER_SUBMISSION = 3
  val PASSING_GRADE = 240

  val all = ArrayBuffer.empty[Learner]

  def create(id: Int, firstStart: Int, firstGrade: Int, reviewBias: Int): Learner = { //TODO: make this factory the only accessible
    val learner = Learner(id, firstStart, firstGrade, reviewBias)
    all ++= Seq(learner)
    learner
  }

  def reviewing(tick: Int) = all.filter(r => r.state == Reviewing && r.start + REVIEW_WINDOW <= tick)

  def working(tick: Int) = all.filter(w => w.state == Working && w.start + Learner.WORK_WINDOW <= tick)

  def waitingToReview = all.filter(_.state == WaitingToReview)
}


object Coursera extends App {
  def simulate(stopTime: Int) = {
    (1 to stopTime).foreach { tick =>
      // Learners publish reviews & move to next state; process them
      Learner.reviewing(tick).foreach(_.publishReview(tick))

      // Learners submit
      Learner.working(tick).foreach(_.createSubmission(tick))

      // Unprocessed Reviews to take effect
      Review.unprocessed.foreach(_.update(tick))

      // Reviewers select submission to review
      Learner.waitingToReview.foreach(r => r.tryReview(tick))
    }
  }

  val inputFile = "courseraInput/input009.txt"
  val lines = Source.fromFile(inputFile).getLines()
  val stopTime = lines.next.toInt
  val learnerCount = lines.next.toInt
  (1 to learnerCount) foreach { i =>
    val Array(learnerId, firstStartTime, firstGrade, reviewBias) = lines.next.split(" ")
    Learner.create(learnerId.toInt, firstStartTime.toInt, firstGrade.toInt, reviewBias.toInt)
  }

  println("Simulate: " + Learner.all.length + " " + stopTime)
  println(Learner.all.mkString("\n"))
  println("=============")
  simulate(stopTime)
  println(">>>>>>>>>>>>>")
  println(Submission.all.mkString("\n"))
}

/*
  README:
  - biasMin=-20, grade improves by 20 after scored as failed (3 reviews), so a learner submits no more than 6 times.
  - #learners < 1000, so there are less than 1000*6 = 6000 submissions (hence 18000 reviews).
  - As a result, ArrayBuffer is used for both Submission.all & Review.all for a simple implementation.
    If #submissions gets to a millions, it makes sense to split all submissions/reviews in different queues
    (e.g. pending, ready, processed etc.)
 */









/*
  Line format: learnId, firstSubmission, firstGrade, reviewBias

 */