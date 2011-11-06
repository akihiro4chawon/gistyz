package akihiro4chawon.github.com.gistyz

import gist4z._
import Gist4z._
import gist4z.objects._

import scalaz._
import Scalaz._

import ErrorHandling._

/** The launched conscript entry point */
class GistyZ extends xsbti.AppMain {
  def run(config: xsbti.AppConfiguration) = {
    Exit(GistyZ.run(config.arguments))
  }
}

object GistyZ {
  /** Shared by the launched version and the runnable version,
   * returns the process status code */
  def run(args: Array[String]): Int = {
    val user = sys.props get "gistyz.user" orError AuthUserNotFound
    val pass = sys.props get "gistyz.pass" orError AuthPassNotFound

    val result = (user |@| pass) { auth.Basic(_, _) } flatMap { a =>
      implicit val authSupply = a
      val cmd = args.headOption orError CommandNotSupplied
      cmd flatMap Commands.apply flatMap {_(args drop 1)}
    }
    
    def mapErrors(errors: NonEmptyList[ErrorT]): NonEmptyList[String] = {
      lazy val usage = Commands.usage map {_._2} mkString "\n"
      errors map {
        case AuthUserNotFound =>
          "github ユーザ名が未設定です。システムプロパティ gistyz.user にユーザ名を設定して下さい。"
        case AuthPassNotFound =>
          "github パスワードが未設定です。システムプロパティ gistyz.pass にパスワードを設定して下さい。"
        case CommandNotSupplied =>
          "コマンドが指定されていません\n" + usage
        case CommandNotFound(cmd) =>
          "そのようなコマンドはありません: "+cmd+"\n"+usage
        case FileNotFound(message, filename) =>
          message+": "+filename
        case InternalError(message) =>
          "おやっ、なにかがおかしいようです："+message
      }
    }
    
    (mapErrors _) <-: result fold (
      {x => println("Success: "+x); 0},
      {x => println(x mkString "\n"); -1} 
    )
  }

  /** Standard runnable class entrypoint */
  def main(args: Array[String]) {
    System.exit(run(args))
  }
}

case class Exit(val code: Int) extends xsbti.Exit


