package mimegenerator

import cats.effect.IO
import io.circe._
import org.http4s.circe._
import org.http4s.client.blaze._
import fs2.Stream
import org.http4s.circe._
import io.circe.generic.semiauto._
import treehugger.forest._, definitions._, treehuggerDSL._
import java.nio.file.Path
import java.io.PrintWriter

final case class MimeDescr(extensions: Option[List[String]], compressible: Option[Boolean])
final case class Mime(mainType: String, secondaryType: String, descr: MimeDescr) {
  // Binary is not on MimeDB. we'll use same mnemonics as in http4s
  def isBinary: Boolean = mainType match {
    case "audio" => true
    case "image" => true
    case "message" => false
    case "text" => false
    case "video" => true
    case "multipart" => false
    case "application" => secondaryType match {
      case "atom+xml" => false
      case "base64" => true
      case "excel" => true
      case "font-woff" => true
      case "gnutar" => true
      case "gzip" => true
      case "hal+json" => true
      case "java-archive" => true
      case "javascript" => false
      case "json" => true
      case "lha" => true
      case "lzx" => true
      case "mspowerpoint" => true
      case "msword" => true
      case "octet-stream" => true
      case "pdf" => true
      case "problem+json" => true
      case "postscript" => true
      case "rss+xml" => false
      case "soap+xml" => false
      case "vnd.api+json" => true
      case "vnd.google-earth.kml+xml" => false
      case "vnd.google-earth.kmz" => true
      case "vnd.ms-fontobject" => true
      case "vnd.oasis.opendocument.chart" => true
      case "vnd.oasis.opendocument.database" => true
      case "vnd.oasis.opendocument.formula" => true
      case "vnd.oasis.opendocument.graphics" => true
      case "vnd.oasis.opendocument.image" => true
      case "vnd.oasis.opendocument.presentation" => true
      case "vnd.oasis.opendocument.spreadsheet" => true
      case "vnd.oasis.opendocument.text" => true
      case "vnd.oasis.opendocument.text-master" => true
      case "vnd.oasis.opendocument.text-web" => true
      case "vnd.openxmlformats-officedocument.presentationml.presentation" => true
      case "vnd.openxmlformats-officedocument.presentationml.slide" => true
      case "vnd.openxmlformats-officedocument.presentationml.slideshow" => true
      case "vnd.openxmlformats-officedocument.presentationml.template" => true
      case "vnd.openxmlformats-officedocument.spreadsheetml.sheet" => true
      case "vnd.openxmlformats-officedocument.spreadsheetml.template" => true
      case "vnd.openxmlformats-officedocument.wordprocessingml.document" => true
      case "vnd.openxmlformats-officedocument.wordprocessingml.template" => true
      case "x-7z-compressed" => true
      case "x-ace-compressed" => true
      case "x-apple-diskimage" => true
      case "x-arc-compressed" => true
      case "x-bzip" => true
      case "x-bzip2" => true
      case "x-chrome-extension" => true
      case "x-compress" => true
      case "x-debian-package" => true
      case "x-dvi" => true
      case "x-font-truetype" => true
      case "x-font-opentype" => true
      case "x-gtar" => true
      case "x-gzip" => true
      case "x-latex" => true
      case "x-rar-compressed" => true
      case "x-redhat-package-manager" => true
      case "x-shockwave-flash" => true
      case "x-tar" => true
      case "x-tex" => true
      case "x-texinfo" => true
      case "x-vrml" => false
      case "x-www-form-urlencoded" => false
      case "x-x509-ca-cert" => true
      case "x-xpinstall" => true
      case "xhtml+xml" => false
      case "xml-dtd" => false
      case "xml" => false
      case "zip" => true
      case _ => false
    }
    case _ => false
  }
  val extensions: Tree = LIST(descr.extensions.getOrElse(Nil).map(LIT))
  val valName: String = s"`$mainType/$secondaryType`"
  def toTree(mediaTypeClass: ClassSymbol): Tree =
    VAL(valName, mediaTypeClass) := NEW(mediaTypeClass, LIT(mainType), LIT(secondaryType), LIT(descr.compressible.getOrElse(false)), LIT(isBinary), extensions)

}

object MimeLoader extends App {
  implicit val MimeDescrDecoder: Decoder[MimeDescr] = deriveDecoder[MimeDescr]

  val readMimeDB: Stream[IO, List[Mime]] =
    for {
      client <- Http1Client.stream[IO]()
      value <- Stream.eval(client.expect[Json]("https://cdn.rawgit.com/jshttp/mime-db/master/db.json"))
      obj <- Stream.emit(value.arrayOrObject(JsonObject.empty, _ => JsonObject.empty, identity))
    } yield {
      obj.toMap.map(x => (x._1.split("/").toList, x._2)).collect {
        case ((m :: s :: Nil), d) => d.as[MimeDescr] match {
          case Right(md) => Some(Mime(m, s, md))
          case Left(_) => None
        }
      }.collect {
        case Some(x) => x
      }.toList.sortBy(m => (m.mainType, m.secondaryType))
    }

  val mimes: Stream[IO, List[Mime]] = for {
    mimes <- readMimeDB
  } yield mimes

  def toTree(topLevelPackge: String, objectName: String, mediaTypeClassName: String)(mimes: List[Mime]): Tree = {
    val all: Tree = (VAL("all", ListClass TYPE_OF TYPE_REF(REF(mediaTypeClassName)))) := LIST(mimes.map(m => REF((m.valName))))
    val mediaTypeClass = RootClass.newClass(mediaTypeClassName)
    val vals: List[Tree] = mimes.map(_.toTree(mediaTypeClass))
    val allVals = vals :+ all
    val privateWithin = topLevelPackge.split("\\.").toList.lastOption.getOrElse("this")
    (((OBJECTDEF(objectName) withFlags(PRIVATEWITHIN(privateWithin))) := BLOCK(IMPORT(s"$topLevelPackge.$mediaTypeClassName") :: allVals))) inPackage(topLevelPackge)
  }

  private def treeToFile(f: Path, t: Tree): Unit = {
    val writer = new PrintWriter(f.toFile)
    writer.write(treeToString(t))
    writer.close()
  }

  /**
   * This method will dowload the MimeDB and produce a file with generated code for http4s
   */
  def toFile(f: Path): IO[Unit] =
    (for {
      m <- mimes
      t <- Stream.emit(toTree("org.http4s", "MimeDB", "MediaType")(m))
      _ <- Stream.emit(treeToFile(f, t))
    } yield ()).compile.drain

  mimes.map(toTree("org.http4s", "MimeDB", "MediaType")).map(x => println(treeToString(x))).compile.drain.unsafeRunSync
}
