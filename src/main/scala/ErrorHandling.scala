package akihiro4chawon.github.com.gistyz

import scalaz._
import Scalaz._

object ErrorHandling {
  //  type ValidationNEL[+E, +X] = Validation[NonEmptyList[E], X]
  class OrError[A](opt: Option[A]) {
    def orError(b: ErrorT): ValidationNEL[ErrorT, A] = opt.cata(_.success, b.fail.liftFailNel)
  }
  implicit def pimpOrFailNel[A](opt: Option[A]): OrError[A] = new OrError(opt)
}

sealed abstract class ErrorT
abstract class AuthNotFound extends ErrorT
case object AuthUserNotFound extends AuthNotFound
case object AuthPassNotFound extends AuthNotFound
abstract class InvalidCommandLine extends ErrorT
case object CommandNotSupplied extends InvalidCommandLine
case class CommandNotFound(cmd: String) extends InvalidCommandLine
//case class CommandArgumentsInvalid() extends InvalidCommandLine
case class FileNotFound(message: String, filename: String) extends ErrorT
case class InternalError(message: String) extends ErrorT