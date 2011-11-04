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
    val pass = sys.props get "gistyz.user" orError AuthPassNotFound

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

object Commands {
  private val cmdMap =
    collection.mutable.Map.empty[String, Command]
  
  def usage: Seq[(String, String)] =
    cmdMap.map(_ :-> (_.usage))(collection.breakOut).sortBy(_._1)
  
  def apply(cmd: String): ValidationNEL[ErrorT, Command] =
    cmdMap get cmd orError(CommandNotFound(cmd))
  
  abstract class Command(commandName: String) {
    cmdMap(commandName) = this
    def apply(args: Seq[String])(implicit a: auth.SomeAuth): ValidationNEL[ErrorT, String]
    final def usage: String = """gistyz """+commandName+argsDesc
    def argsDesc: String
  }
  
  val Post = new PostBase("post", true)
  val PostPrivate = new PostBase("post-private", false)
  val Clone = new Clone
  
  // TODO handle errors exhaustively
  class Clone extends Command("clone") {
    def argsDesc = """[dest directory]
      clone all your gists to specified directory"""
    
    def apply(args: Seq[String])(implicit a: auth.SomeAuth) = {
      import net.liftweb.json.scalaz.JsonScalaz.Result
      
      val dir = new java.io.File(args.head)
      // eagerly validate json and then write files to disk
      Gists.mine().flatMap {
        _.map(Gists byId _.id).sequence[Result, ExtGist]
      }.`<-:` {_ map {e => InternalError(e.toString)}}.`:->` { gists =>
        gists map {g => writeExtGistToDisk(dir, g)}
      }.`:->` {_ => "maybe successfully cloned" }
    }
    
    private def writeExtGistToDisk(dir: java.io.File, eg: ExtGist) {
      import java.io.FileWriter
      implicit val FileWriteResouce: Resource[FileWriter] = resource(_.close)
      
      val gistDir = new java.io.File(dir, eg.id)
      confirmPath(gistDir)
      for {
        gf <- eg.files
        filePath = new java.io.File(gistDir, gf.filename)
      } withResource(new FileWriter(filePath), (_: FileWriter) write gf.content)
    }
    
    private def confirmPath(dir: java.io.File): Boolean =
      dir.exists || (Iterator continually dir.mkdirs take 100 exists identity)
      // Need a retry bacause mkdirs() fails due to race condition
      // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4742723
  }
  
  class PostBase(commandName: String, isPublic: Boolean) extends Command(commandName) {
    def argsDesc = """ [--description description] [file1] [file2] ...
      post """+isPublic.fold("publicly", "privately")+""" files to gist"""
      
    def apply(args: Seq[String])(implicit a: auth.SomeAuth) = {
      import scala.util.control.Exception._
      import scala.io.Source
      import java.io.{File, FileNotFoundException}
      
      def parseArg(args: List[String]): (String, List[String]) = args match {
        case "--description" :: description :: xs =>
          (description, nil[String]) |+| parseArg(xs)
        case file :: xs =>
          (mzero[String], List(file)) |+| parseArg(xs)
        case Nil => mzero[(String, List[String])]
      }
      val (description, files) = parseArg(args.toList)
      
      val filesValidation = files.map { filename =>
        validation {
          catching(classOf[FileNotFoundException]).either {
            filename -> Source.fromFile(new File(filename))
          }.`<-:` { e => FileNotFound(e.getMessage, filename) }
        } liftFailNel
      }.sequence[PartialApply1Of2[ValidationNEL, ErrorT]#Apply, (String, Source)] :-> {
        _ map { case (filename, source) =>
          GistCreateFile(filename replaceAllLiterally ("\\", "_"), source.mkString)
        }
      }

      filesValidation.flatMap { files =>
        Gists.create(GistCreate(
          description = description,
          public = true,
          files = files)).`<-:`{_ map {e => InternalError(e.toString)}}
      }.`:->` {
        "successfully created gist: " + _
      }
    }
  }
}


