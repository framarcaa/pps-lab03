package it.unibo.pps.u03

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import it.unibo.pps.u02.Modules.{Person, isStudent}
import it.unibo.pps.u02.Modules.Person.*

import scala.annotation.tailrec

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

  @tailrec
  def foldLeft[A, B](s: Sequence[A])(default: B)(mapper: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(mapper(default, h))(mapper)
    case Nil() => default

  def distinctCourse(s: Sequence[Person]): Int =
    val teacherCourses = map(filter(s)(!isStudent(_)))(getCourse)
    val uniqueCourses = distinct(teacherCourses)
    foldLeft(uniqueCourses)(0)((acc, _) => acc + 1)


@main def mainCourse(): Unit =
  val people: Sequence[Person] = Cons(Student("mario", 2015), Cons(Teacher("luigi", "pps"), Cons(Teacher("peach", "pps"), Nil())))
  println(Task2.getTeacherCourse(people)) // Should print Cons("pps", Cons("pps", Nil()))
  println(Task2.getTeacherCourseFlatMap(people)) // Should print Cons("pps", Cons("pps", Nil()))

@main def mainLeftFold(): Unit =
  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(Task2.foldLeft(lst)(0)(_ - _)) // -16

@main def mainDistinctCourse(): Unit =
  val people: Sequence[Person] = Cons(Student("mario", 2015), Cons(Teacher("luigi", "pps"), Cons(Teacher("peach", "pps"), Cons(Teacher("apple", "pcd"), Nil()))))
  println(Task2.distinctCourse(people)) // Should print 1
