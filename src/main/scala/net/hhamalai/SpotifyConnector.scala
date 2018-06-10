package net.hhamalai
import java.io.{BufferedWriter, File, FileWriter}
import java.util.{Base64, Calendar}
import java.nio.charset.StandardCharsets
import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.time.ZoneOffset

import cats.effect.IO
import com.softwaremill.sttp._
import com.softwaremill.sttp.circe._
import doobie.Transactor
import io.circe.Json
import io.circe.parser._
import io.circe.generic.auto._

import scalatags.Text.all._

trait TimeLineItem
case class Addition(track: Track) extends TimeLineItem
case class Removal(track: Track) extends TimeLineItem

case class Track(id: String, name: String, href: String, artist: String, artistHref: String, added_by: String, added_at: Timestamp, removed_at: Option[Timestamp])

case class AuthenticationResponse(access_token: String, token_type: String, expires_in: Int, scope: String)

trait SpotifyConnector {
  val token: Option[String]
}
case class Unauthenticated(token: Option[String]) extends SpotifyConnector
case class Authenticated(token: Option[String]) extends SpotifyConnector

object Authenticated {
  def foo() = ""
}

object SpotifyConnector {
  implicit val backend = HttpURLConnectionBackend()

  def apply() = {
    println("Client id", System.getenv("CLIENT_ID"))
    val base64Encoded = Base64.getEncoder.encodeToString(s"${System.getenv("CLIENT_ID")}:${System.getenv("CLIENT_SECRET")}"
      .getBytes(StandardCharsets.UTF_8))

    val request = sttp.post(uri"https://accounts.spotify.com/api/token")
      .headers(("Authorization", s"Basic ${base64Encoded}"))
      .body(("grant_type", "client_credentials"))
      .response(asJson[AuthenticationResponse])
    request.send().unsafeBody match {
      case Right(response) => Authenticated(Some(response.access_token))
      case Left(_) => Unauthenticated(None)
    }
  }

  def getTracInfo(json: Json) = {
    val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    val added_at = json.hcursor.get[String]("added_at") match {
      case Right(x) => {
        val d = format.parse(x.toString());
        Timestamp.from(d.toInstant)
      }
      case Left(_) => new Timestamp(0)
    }
    val added_by = json.hcursor.downField("added_by").get[String]("id") match {
      case Right(x) => x
      case Left(_) => ""
    }
    val track  = json.hcursor.downField("track")
    val trackId = track.get[String]("id") match {
      case Right(x) => x
      case Left(_) => ""
    }
    val trackHref = track.get[String]("href") match {
      case Right(x) => x
      case Left(_) => ""
    }
    val trackName = track.get[String]("name") match {
      case Right(x) => x
      case Left(_) => ""
    }

    val artist = track.downField("artists").downArray
    val artistName = artist.get[String]("name") match {
      case Right(x) => x
      case Left(_) => ""
    }
    val artistHref = artist.get[String]("href") match {
      case Right(x) => x
      case Left(_) => ""
    }
    Track(trackId, trackName, trackHref, artistName, artistHref, added_by, added_at, None)
  }

  def requestPlaylist(token: String, user: String, playlist: String, offsetCorr: Int = 0, limit: Int = 100) = {
    val offset  = offsetCorr * 100
    val request = sttp.get(uri"https://api.spotify.com/v1/users/$user/playlists/$playlist/tracks?offset=$offset&limit=$limit")
      .headers(("Authorization", s"Bearer $token"))
    parse(request.send().unsafeBody).getOrElse(Json.Null)
  }

  def getTracks(token: String, user: String, playlist: String) = {
    val doc = requestPlaylist(token, user, playlist, 0, 1)
    val totalItems = doc.hcursor.get[Int]("total") match {
      case Right(total) => total
      case Left(_) => 0
    }
    val range = 0 to (totalItems / 100)
    val combinedResults: Seq[Json] = range.map(x => {
      requestPlaylist(token, user, playlist, x)
    })
    combinedResults.flatMap(x => x.hcursor.downField("items").focus match {
      case Some(items) => items.asArray match {
        case Some(arr) => arr.map(getTracInfo)
      }
      case _ => throw new RuntimeException("No track items in response")
    })
  }

  def render (items: Seq[TimeLineItem]): Unit = {
    val lastUpdate = new Timestamp(Calendar.getInstance().getTimeInMillis).toInstant.atOffset(ZoneOffset.ofHours(3)).toString
    val text = html(
      head(

      ),
      body(
        div(
          items.map({
            case Addition(track) => div(
              div("Added: ", track.added_at.toInstant.atOffset(ZoneOffset.ofHours(3)).toString, " ",  a(href := track.href) (track.name), " by ", a(href := track.artistHref) (track.artist), " (", track.added_by, ") ")
            )
            case Removal(track) => div(
              div("Removed: ", track.removed_at.get.toInstant.atOffset(ZoneOffset.ofHours(3)).toString, " ", a(href := track.href) (track.name), " by ", a(href := track.artistHref) (track.artist), " (", track.added_by, ") ")

            )
          })
        ),
        div(
          s"Last update: $lastUpdate"
        )
      )
    ).toString()

    val file = new File(System.getenv("RENDER_OUTPUT"))
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }

  def mergeLists(adds: List[Addition], rems: List[Removal], cmp: (Addition, Removal) => Int): List[TimeLineItem] = {
    import scala.annotation.tailrec
    @tailrec
    def merge[T](res: List[TimeLineItem], adds: List[Addition], rems: List[Removal], cmp: (Addition, Removal) => Int): List[TimeLineItem] = {
      (adds, rems) match {
        case (Nil, Nil) => res
        case (_, Nil)   => adds.reverse ++ res
        case (Nil, _)   => rems.reverse ++ res
        case (ahead :: atail, rhead :: rtail) =>
          if (cmp(ahead, rhead) <= 0) {
            merge(ahead +: res, atail, rems, cmp)
          } else {
            merge(rhead +: res, adds, rtail, cmp)
          }
      }
    }
    merge(List(), adds, rems, cmp).reverse
  }

  def main (args: Array[String]): Unit = {
    if (args.length > 0) {
      DB.initializeDB
    } else {
      process
    }
  }

  private def process = {
    val xa = Transactor.fromDriverManager[IO](
      "org.sqlite.JDBC", s"jdbc:sqlite://${System.getenv("DBPATH")}"
    )
    val y = xa.yolo
    import y._

    val tracks = SpotifyConnector() match {
      case Unauthenticated(_) => throw new RuntimeException("Unauthenticated")
      case Authenticated(Some(token)) => SpotifyConnector.getTracks(token, System.getenv("USER"), System.getenv("PLAYLIST"))
    }

    val currentIDsInDB: Set[String] = DB.currentTracks.compile.toList.unsafeRunSync.map(_.id).toSet
    val currentPlaylistIDs: Set[String] = tracks.map(_.id).toSet

    val added = tracks.filter(t => currentPlaylistIDs.diff(currentIDsInDB).contains(t.id))
    val removed = currentIDsInDB.diff(currentPlaylistIDs)

    DB.insertMany(added.toList).quick.unsafeRunSync
    removed.toList.foreach(t => DB.remove(t).quick.unsafeRunSync)

    val addOrder = DB.allTracks.compile.toList.unsafeRunSync.map(track => Addition(track))
    val removeOrder = DB.deletedTracks.compile.toList.unsafeRunSync.map(track => Removal(track))
    val timeLineItems = mergeLists(addOrder, removeOrder, (a: Addition, b: Removal) => (a, b) match {
      case (a, b) if b.track.removed_at.isEmpty => -1
      case (a, b) => a.track.added_at.compareTo(b.track.removed_at.get) * -1
      case _ => 0
    })
    render(timeLineItems)
  }
}
