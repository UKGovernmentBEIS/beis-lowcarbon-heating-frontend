/*
 * Copyright (C) 2016  Department for Business, Energy and Industrial Strategy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package services

import com.google.inject.Inject
import config.Config
import models._
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient
import play.api.libs.json.JsObject

import scala.concurrent.{ExecutionContext, Future}


class UserURLs(baseUrl: String) {

  def login : String = s"$baseUrl/user/login"
  def register: String = s"$baseUrl/user/register"
  def forgotpassword: String = s"$baseUrl/user/forgotpassword"
  def resetpassword: String = s"$baseUrl/user/resetpassword"
}

class UserService @Inject()(val ws: WSClient)(implicit val ec: ExecutionContext)
  extends UserOps with RestService {

  val baseUrl = Config.config.business.baseUrl
  val urls = new UserURLs(baseUrl)

  override def login(doc: JsObject) : Future[Option[User]] =
  postWithResult[User, JsObject](urls.login, doc)

  override def register(doc: JsObject): Future[Option[String]] =
    postWithResult[String, JsObject](urls.register, doc)

  override def forgotpassword(doc: JsObject): Future[Option[String]] =
    postWithResult[String, JsObject](urls.forgotpassword, doc)

  override def resetpassword(doc: JsObject): Future[Option[String]] =
    postWithResult[String, JsObject](urls.resetpassword, doc)
 }



