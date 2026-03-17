package it.unibo.pps.u03

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import it.unibo.pps.u02.Modules.{Person, isStudent}
import it.unibo.pps.u02.Modules.Person.*

object Task2:
  private def getCourse(p: Person): String = p match
    case Student(_, _) => ""
    case Teacher(_, c) => c

  //need to combine filter and map to get the course of the teachers
  def getTeacherCourse(p: Sequence[Person]): Sequence[String] =
    map(filter(p)(!isStudent(_)))(t => getCourse(t))

  def getTeacherCourseFlatMap(p: Sequence[Person]): Sequence[String] =
    flatMap(p) {
      case Student(_, _) => Nil()
      case Teacher(_, c) => Cons(c, Nil())
    }

@main def run(): Unit =
  val people: Sequence[Person] = Cons(Student("mario", 2015), Cons(Teacher("luigi", "pps"), Cons(Teacher("peach", "pps"), Nil())))
  println(Task2.getTeacherCourse(people)) // Should print Cons("pps", Cons("pps", Nil()))
  println(Task2.getTeacherCourseFlatMap(people)) // Should print Cons("pps", Cons("pps", Nil()))
