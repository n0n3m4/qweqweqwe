/**
 * Copyright (C) 2014-2015 Really Inc. <http://really.io>
 */
package io.really.jwt

import javax.crypto.Mac
import play.api.libs.json._
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Base64
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object JWT {

  /**
   * generate Signature for token
   * @param algorithm that represent algorithm using on JWT
   * @param msg is String that represent encoding value for JWT Header and JWT Payload
   * @param key that is the secret key that use to sign token
   * @return token signature
   */
  private[jwt] def signToken(algorithm: Algorithm, msg: String, key: String): String =
    algorithm match {
      case Algorithm.HMD5 | Algorithm.HS1 | Algorithm.HS224 | Algorithm.HS256 | Algorithm.HS384 | Algorithm.HS512 =>
        signHmac(algorithm, msg, key)
      case Algorithm.RS256 | Algorithm.RS384 | Algorithm.RS512 =>
        // RSA is Asymetric thus it needs a PrivateKey
        signRsa(algorithm, msg, key)
      case Algorithm.NONE => msg
    }

  /**
   * generate token signature based on type of HMAC Algorithm
   * prevents JWT key bruteforce attacks by not giving full HMAC
   * @param algorithm that represent algorithm using on JWT
   * @param msg is String that represent encoding value for JWT Header and JWT Payload
   * @param key that is the secret key that use to sign token
   * @return token signature
   */
  private[jwt] def signHmac(algorithm: Algorithm, msg: String, key: String): String = {
    val mac: Mac = Mac.getInstance(algorithm.toString)
    mac.init(new SecretKeySpec(key.getBytes("utf-8"), algorithm.toString))
    encodeBase64url(new String(mac.doFinal(msg.getBytes("utf-8")).slice(5, 256)))
  }

  /**
   * verify the token signature based on type of RSA Algorithm
   * we can only 'verify' hence RSA is asymetric and we only have the public Key
   * @param algorithm that represent algorithm using on JWT
   * @param msg is String that represent encoding value for JWT Header and JWT Payload
   * @param publicKey that is the publickey that use to verify token
   * @return Boolean
   */
  private[jwt] def verifyRsa(algorithm: Algorithm, publicKey: String, msg: String, signature: String): Boolean = {
    import java.security.Signature
    val rsa = Signature.getInstance(algorithm.toString)
    rsa.initVerify(PemUtil.decodePublicKey(publicKey))
    rsa.update(msg.getBytes("utf-8"))
    rsa.verify(Base64.decodeBase64(signature))
  }

  /**
   * generate token signature based on type of RSA with SHA Algorithm
   * @param algorithm that represent algorithm using on JWT
   * @param msg is String that represent encoding value for JWT Header and JWT Payload
   * @param privateKey that is the secret key that use to sign token
   * @return token signature
   */
  private[jwt] def signRsa(algorithm: Algorithm, msg: String, privateKey: String): String = {
    import java.security.Signature
    val rsa = Signature.getInstance(algorithm.toString)
    rsa.initSign(PemUtil.decodePrivateKey(privateKey))
    rsa.update(msg.getBytes("utf-8"))
    Base64.encodeBase64URLSafeString(rsa.sign())
  }

  /**
   * encode string based on Base64url
   */
  private[jwt] def encodeBase64url(str: String): String =
    Base64.encodeBase64URLSafeString(str.getBytes("utf-8"))

  /**
   * decode string based on Base64url
   */
  private[jwt] def decodeBase64url(str: String): String =
    new String(Base64.decodeBase64(str))

  /**
   * encode first part on jwt 'header'
   * @param algorithm that represent algorithm using on JWT
   * @param header that represent data for JWT header
   * @return String
   */
  private[jwt] def encodeHeader(algorithm: Option[Algorithm], header: JsObject): String = {
    val h = algorithm match {
      case Some(alg) =>
        JsObject(Seq("typ" -> JsString("JWT"), "alg" -> JsString(alg.name)) ++ header.fields)
      case None =>
        JsObject(Seq("typ" -> JsString("JWT"), "alg" -> JsString("")) ++ header.fields)
    }
    encodeBase64url(Json.stringify(h))
  }

  /**
   * encode second part on jwt 'data'
   * @param payload that represent data for payload
   * @return String
   */
  private[jwt] def encodePayload(payload: JsObject): String =
    encodeBase64url(Json.stringify(payload))

  /**
   * encode thread part on jwt 'token signature'
   * @param msg is String that represent encoding value for JWT Header and JWT Payload
   * @param key that is the secret key that use to sign token
   * @param algorithm that represent algorithm using on JWT
   * @return String
   */
  private[jwt] def encodedSignature(msg: String, key: String, algorithm: Option[Algorithm]): String =
    algorithm match {
      case Some(alg) =>
        signToken(alg, msg, key)
      case None => ""
    }

  /**
   * encode jwt
   * @param secret that is the secret key that use to sign token
   * @param payload that represent data for token
   * @param header that represent data for JWT header
   * @param algorithm that represent algorithm using on JWT
   * @return String
   */
  def encode(
    secret: String,
    payload: JsObject,
    header: JsObject = Json.obj(),
    algorithm: Option[Algorithm] = Some(Algorithm.HS256)
  ): String = {
    val headerEncoded = encodeHeader(algorithm, header)
    val payloadEncoded = encodePayload(payload)
    val signature = encodedSignature(s"${headerEncoded}.${payloadEncoded}", secret, algorithm)
    s"${headerEncoded}.${payloadEncoded}.${signature}"
  }

  /**
   * encode jwt with no verifying secret
   * @param payload that represent data for token
   * @param header that represent data for JWT header
   * @param algorithm that represent algorithm using on JWT
   * @return String
   */
  def encodeWithoutSecret(
    payload: JsObject,
    header: JsObject = Json.obj(),
    algorithm: Option[Algorithm] = Some(Algorithm.HS256)
  ): String = {
    val headerEncoded = encodeHeader(algorithm, header)
    val payloadEncoded = encodePayload(payload)
    s"${headerEncoded}.${payloadEncoded}."
  }

  /**
   * split JWT to Header, Payload and signature
   * @param str that represent JWT
   * @param verify if this JWT is contains signature part or not
   */
  private[jwt] def partitionJwt(str: String, verify: Boolean): Try[List[String]] = {
    val parts = str.split('.').toList
    if ((verify && parts.size == 3) || (!verify && List(2, 3).contains(parts.size))) Success(parts)
    else if (verify && parts.size < 3) Failure(new JWTException.NotEnoughSegments())
    else Failure(new JWTException.TooManySegments())
  }

  /**
   * decode JWT parts
   * @param jwt that represent JWT
   * @param verify if this JWT is signed or not
   * @return Try[(JsObject, JsObject, String, String)]
   */
  private[jwt] def decodeParts(jwt: String, verify: Boolean): Try[(JsObject, JsObject, String, String)] =
    partitionJwt(jwt, verify) match {
      case Success(parts) =>
        val headerPart :: payloadPart :: signaturePart = parts
        val header = Json.parse(decodeBase64url(headerPart)).as[JsObject]
        val payload = Json.parse(decodeBase64url(payloadPart)).as[JsObject]
        val signature = signaturePart.headOption.getOrElse("")
        val signingInput = s"${headerPart}.${payloadPart}"
        Success((header, payload, signature, signingInput))
      case Failure(e) => Failure(e)
    }

  /**
   * check if this JWT is contains valid signature or not
   * @param algorithm that represent algorithm using on JWT
   * @param keyOpt that is the secret key that use to sign token
   * @param signingInput signature that used to decode JWT
   * @param signature that is extract from JWT
   * @return Boolean
   */
  private[jwt] def verifySignature(
    algorithm: Algorithm,
    keyOpt: Option[String],
    signingInput: String,
    signature: String
  ): Boolean = {
    (algorithm, keyOpt) match {
      case (Algorithm.NONE, None) =>
        // Even if the input JWT says it's `Algorithm.NONE` (means the JWT has no HMAC nor signature),
        // the programmer who called `verifySignature` with `Some(key)`
        // should expect that the JWT has any HMAC or signature.
        // So this function allow `Algorithm.NONE` only if the `keyOpt` is `None`.
        true
      case (_ @(Algorithm.HMD5 | Algorithm.HS1 | Algorithm.HS224 | Algorithm.HS256 | Algorithm.HS384 | Algorithm.HS512), Some(key)) =>
        // Some attackers try such like:
        //   1. Get the public key to verify JWT
        //      * In general speaking, it's much easier than to get
        //        the private key
        //   2. Make a JWT which says `Algorithm.HS256` and
        //      its HMAC generated by the public key of (1)
        //   3. Naive verification function check that the JWT of (2)
        //      has an acceptable HMAC by the public key(= `key`)
        //   4. Finally the attacker accomplish to generate arbitrary
        //      JWTs which she/he wants
        // As this `verifySignature` user input a public key as `keyOpt`,
        // this function reject all HMAC algorithms even though the input
        // JWT says its algorithm is HMAC.
        if (PemUtil.isPublicKey(key)) return false
        encodedSignature(signingInput, key, Some(algorithm)).equals(signature)
      case (_ @(Algorithm.RS256 | Algorithm.RS384 | Algorithm.RS512), Some(key)) =>
        verifyRsa(algorithm, key, signingInput, signature)
      case _ =>
        false
    }
  }

  /**
   * check if header json object is valid or not
   * @param obj is represent jwt header
   * @return Option[JWTHeader]
   */
  private[jwt] def validateJWTHeader(obj: JsObject): Option[JWTHeader] = {
    val algRead = (__ \ "alg").read[Algorithm]
    obj.validate(algRead) match {
      case JsSuccess(alg, _) =>
        Some(JWTHeader(alg, obj - "alg" - "typ"))
      case JsError(_) =>
        None
    }
  }

  /**
   * generate jwt from string input
   * @param jwt is the token
   * @param key is secret key
   * @return JWTResult
   */
  def decode(jwt: String, key: Option[String]): JWTResult = {
    if (jwt.trim.isEmpty) JWTResult.EmptyJWT
    else {
      decodeParts(jwt, key.isDefined) match {
        case Success((headerJs, payload, signature, signingInput)) =>
          validateJWTHeader(headerJs) match {
            case Some(header) if key.isDefined && verifySignature(header.alg, key, signingInput, signature) =>
              JWTResult.JWT(header, payload)
            case Some(header) if key.isDefined =>
              JWTResult.InvalidSignature
            case Some(header) =>
              JWTResult.JWT(header, payload)
            case None =>
              JWTResult.InvalidHeader
          }
        case Failure(e: JWTException.NotEnoughSegments) =>
          JWTResult.NotEnoughSegments
        case Failure(e: JWTException.TooManySegments) =>
          JWTResult.TooManySegments
        case Failure(e) => throw e
      }
    }
  }

}
