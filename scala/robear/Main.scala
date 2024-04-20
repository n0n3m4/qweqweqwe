package robear
import cats.effect.{ExitCode, IO, IOApp}
import com.twitter.finagle.http.{Request, Response, Cookie}
import scala.io.Source
import com.twitter.io.Buf
import java.io.File
import play.api.libs.json.{Json, JsObject}
import cats.implicits._
import scala.util.parsing.json._
import io.really.jwt._
import io.finch._
import robear.filedb._
import java.security.SecureRandom
import scalaj.http.Http

object Main extends IOApp with Endpoint.Module[IO] {
  private val db = new FileDB("db")
  private val jwtKey = generateRandomKey(16)

  private def generateRandomKey(length: Int): String = {
    val chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val sb = new StringBuilder
    val random = new SecureRandom()
    for (_ <- 1 to length) {
      val randomNum = random.nextInt(chars.length)
      sb.append(chars.charAt(randomNum))
    }
    sb.toString
  }

  private def respond_with_json(json: String): Response = {
    val res = Response()
    res.contentType = "application/json; charset=UTF-8"
    res.content = Buf.Utf8(json)
    res
  }

  private val apiRegister: Endpoint[IO, Response] = post("api" :: "register" :: stringBody) { (json: String) =>
    val alphanumericRegex = "^[A-Za-z0-9]+$".r

    def isValidCredential(credential: String): Boolean =
      credential.length >= 9 && alphanumericRegex.matches(credential)

    JSON.parseFull(json) match {
      case Some(v: Map[String, String]) =>
        val login = v.getOrElse("login", throw new Exception)
        val password = v.getOrElse("password", throw new Exception)

        if (isValidCredential(login) && isValidCredential(password)) {
          val result = query(db, "robear", select("login") from "users" where equal("login", login))
          result match {
            case Some(v) if v.get("login").exists(_.isEmpty) =>
              query(db, "robear", insert("users") values ("login" -> login, "password" -> password, "role" -> "ro"))
              respond_with_json("""{"success": true}""")
            case _ => respond_with_json("""{"success": false}""")
          }
        } else {
          respond_with_json("""{"success": false}""")
        }
      case _ => throw new Exception
    }
  } handle {
    case e: Exception => InternalServerError(e)
  }

  private val apiLogin: Endpoint[IO, Response] = post("api" :: "login" :: stringBody) { (json: String) =>
    JSON.parseFull(json) match {
      case Some(v: Map[String, String]) =>
        val login = v.getOrElse("login", throw new Exception)
        val password = v.getOrElse("password", throw new Exception)

        val result = query(db, "robear", select("login", "password", "role") from "users" where (equal("login", login), equal("password", password)))
        result match {
          case Some(v) if v.get("login").exists(_.nonEmpty) && v.get("password").exists(_.nonEmpty) =>
            val role = v.getOrElse("role", throw new Exception)(0)
            val payload = Json.obj("login" -> login, "role" -> role)
            val jwt = JWT.encode(jwtKey, payload)
            val response = respond_with_json(s"""{"success": true, "role": "$role"}""")
            val cookie = new Cookie("jwt", jwt)
            response.cookies.add("jwt", cookie)
            response
          case _ => respond_with_json("""{"success": false}""")
        }
      case _ => throw new Exception
    }
  } handle {
    case e: Exception => InternalServerError(e)
  }

  private val apiLogout: Endpoint[IO, Response] = post("api" :: "logout") {
    val response = respond_with_json(s"""{"success": true}""")
    val cookie = new Cookie("jwt", "")
    response.cookies.add("jwt", cookie)
    response
  } handle {
    case e: Exception => InternalServerError(e)
  }

  private val apiStatus: Endpoint[IO, Response] = get("api" :: "status" :: cookie("jwt")) { (jwtCookie: Cookie) =>
    JWT.decode(jwtCookie.value, Some(jwtKey)) match {
      case JWTResult.JWT(_, payload: JsObject) =>
        query(db, "robear", select("type", "value") from "instruments") match {
          case Some(v) =>
            val types = v.getOrElse("type", throw new Exception)
            val values = v.getOrElse("value", throw new Exception)
            val jsonArray = types.zip(values).map { case (t, v) =>
              s"""{"type": "$t", "value": "$v"}"""
            }.mkString("[", ",", "]")
            respond_with_json(s"""{"success": true, "status": $jsonArray}""")
          case _ => throw new Exception
        }
      case _ => respond_with_json(s"""{"success": false, "message": "Not logged in"}""")
    }
  } handle {
    case e: Exception => InternalServerError(e)
  }

  private val apiSetup: Endpoint[IO, Response] = post("api" :: "setup" :: cookie("jwt") :: stringBody) { (jwtCookie: Cookie, json: String) =>
    JWT.decode(jwtCookie.value, Some(jwtKey)) match {
      case JWTResult.JWT(_, payload: JsObject) =>
        val role = (payload \ "role").as[String]
        if (role == "readwrite") {
          JSON.parseFull(json) match {
            case Some(v: Map[String, String]) =>
              val mode = v.getOrElse("mode", throw new Exception)
              if (mode == "Firefighter") {
                val flag = "tctf{XXXXXXXXXXXXXXXXXXXXXXXXXX}"
                respond_with_json(s"""{"success": true, "flag": "Congratulations! Here is your reward: $flag"}""")
              } else {
                respond_with_json("""{"success": false, "flag": "Bear is now doing some other stupid thing"}""")
              }
            case _ => throw new Exception
          }
        } else {
          respond_with_json("""{"success": false, "flag": "Your access level prevents you from setting up the system"}""")
        }
      case _ => throw new Exception
    }
  } handle {
    case e: Exception => InternalServerError(e)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    if (!(new File("db/robear")).exists()) {
      db.createDatabase("robear")
      query(db, "robear", create("users") cols("login", "password", "role"))
      query(db, "robear", create("instruments") cols("type", "value"))
      query(db, "robear", insert("instruments") values("type" -> "Temperature", "value" -> "1100 *C") )
      query(db, "robear", insert("instruments") values("type" -> "CO2", "value" -> "97%") )
      query(db, "robear", insert("instruments") values("type" -> "Noise", "value" -> "85 dB") )
    }
    val apiRoute = apiRegister :+: apiLogin :+: apiLogout :+: apiStatus :+: apiSetup
    Bootstrap[IO]
      .serve[Text.Plain](apiRoute)
      .listen(":3000").useForever
  }
}
