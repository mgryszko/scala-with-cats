package scalacats.chapter4

import cats.data.Reader

case class Db(usernames: Map[Int, String], passwords: Map[String, String])

object DbOps {
  type DbReader[B] = Reader[Db, B]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader[Db, Option[String]](_.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] = Reader[Db, Boolean](_.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      loggedIn <- username.fold(Reader[Db, Boolean](_ => false))(checkPassword(_, password))
    } yield loggedIn

}

