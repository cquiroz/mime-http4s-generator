package mimegenerator

import cats.effect.IO
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.client.blaze._
import fs2.Stream
import org.http4s.circe._
import io.circe.generic.semiauto._

final case class MimeDescr(extensions: Option[List[String]], compressible: Option[Boolean])
final case class Mime(mimeType: String, descr: MimeDescr)

object MimeLoader extends App {
  implicit val MimeDescrDecoder: Decoder[MimeDescr] = deriveDecoder[MimeDescr]
  val program = for {
    client <- Http1Client.stream[IO]()
    value <- Stream.eval(client.expect[Json]("https://cdn.rawgit.com/jshttp/mime-db/master/db.json"))
    obj <- Stream.emit(value.arrayOrObject(JsonObject.empty, _ => JsonObject.empty, identity))
  } yield {
    obj.toMap.map {
      case (t, d) => d.as[MimeDescr] match {
        case Right(md) => Some(Mime(t, md))
        case Left(_) => None
      }
    }
  }
  program.run.unsafeRunSync
}
