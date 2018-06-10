package net.hhamalai

import java.sql.Timestamp
import java.util.Calendar

import doobie._
import doobie.implicits._
import cats.effect.IO
import cats.implicits._



object DB {
  val xa = Transactor.fromDriverManager[IO](
    "org.sqlite.JDBC", s"jdbc:sqlite://${System.getenv("DBPATH")}"
  )
  val y = xa.yolo
  import y._


  val drop =
    sql"""
   DROP TABLE IF EXISTS tracks
  """.update.run

  val create =
    sql"""
    CREATE TABLE IF NOT EXISTS tracks (
      id   int,
      name text,
      href text,
      artist text,
      artistHref text,
      added_by text,
      added_at timestamp,
      removed_at timestamp
    )
  """.update.run

  def insert(id: String, name: String, href: String, added_by: String, artist: String, artistHref: String, added_at: Timestamp) =
    sql"insert into tracks (id, name, href, artist, artistHref, added_by, added_at) values ($id, $name, $href, $artist, $added_by, $added_at)".update.run

  def insert(track: Track) =
    sql"insert into tracks (id, name, href, artist, artistHref, added_by, added_at) values (${track.id}, ${track.name}, ${track.href}, ${track.artist}, ${track.artistHref},  ${track.added_by}, ${track.added_at})".update.run

  def insertMany(ts: List[Track]): ConnectionIO[Int] = {
    val sql = "insert into tracks (id, name, href, artist, artistHref, added_by, added_at, removed_at) values (?, ?, ?, ?, ?, ?, ?, ?)"
    Update[Track](sql).updateMany(ts)
  }

  def removeMany(ts: List[String]): ConnectionIO[Int] = {
    val removed_at = new Timestamp(Calendar.getInstance().getTimeInMillis)
    var sql = "update tracks set removed_at = $removed_at where id = ?"
    Update[String](sql).updateMany(ts)
  }

  def remove(id: String) = {
    val removed_at = new Timestamp(Calendar.getInstance().getTimeInMillis)
    sql"update tracks set removed_at = $removed_at where id = $id".update.run
  }

  def remove(track: Track) = {
    val removed_at = new Timestamp(Calendar.getInstance().getTimeInMillis)
    sql"update tracks set removed_at = $removed_at where id = ${track.id}".update.run
  }

  def allTracks = {
    sql"select id, name, href, artist, artistHref, added_by, added_at, removed_at from tracks order by added_at desc".query[Track].stream.transact(xa)
  }
  def currentTracks = {
      sql"select id, name, href, artist, artistHref, added_by, added_at, removed_at from tracks where removed_at is null order by added_at desc".query[Track].stream.transact(xa)
  }
  def deletedTracks = {
    sql"select id, name, href, artist, artistHref, added_by, added_at, removed_at from tracks where removed_at is not null order by removed_at desc".query[Track].stream.transact(xa)
  }

  def initializeDB: Unit = {
    (drop, create).mapN(_ + _).transact(xa).unsafeRunSync
  }
}
