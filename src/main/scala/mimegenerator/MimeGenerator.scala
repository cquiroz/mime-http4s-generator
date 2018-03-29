package mimegenerator

import cats.effect.IO
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.client.blaze._
import fs2.Stream

object MimeLoader extends App {
  val program = for {
    client <- Http1Client.stream[IO]()
    value <- Stream.eval(client.expect[String]("https://cdn.rawgit.com/jshttp/mime-db/master/db.json"))
    _ <- Stream.emit(println(value))
  } yield ()
  program.run.unsafeRunSync
}
