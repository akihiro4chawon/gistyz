package akihiro4chawon.github.com.gistyz

import gist4z._
import Gist4z._
import gist4z.objects._

import scalaz._
import Scalaz._

import ErrorHandling._

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


