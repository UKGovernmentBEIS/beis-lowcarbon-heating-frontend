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

package actions

import javax.inject.Inject

import models.{Application, ApplicationDetail, ApplicationId}
import play.api.i18n.MessagesApi
import play.api.mvc.Results._
import play.api.mvc._
import services.ApplicationOps

import scala.concurrent.{ExecutionContext, Future}

import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._


case class AppDetailRequest[A](appDetail: ApplicationDetail, request: Request[A]) extends WrappedRequest[A](request)

class AppDetailAction @Inject()(applications: ApplicationOps, msg: MessagesApi)(implicit ec: ExecutionContext) {
  implicit val messages = Messages

  def apply(id: ApplicationId): ActionBuilder[AppDetailRequest] =
    new ActionBuilder[AppDetailRequest] {
      override def invokeBlock[A](request: Request[A], next: (AppDetailRequest[A]) => Future[Result]): Future[Result] = {
        implicit val sessionUser: Option[String] = {
          for (suser<- request.session.get("username"))
            yield suser
        }
        applications.detail(id).flatMap {
          case Some(app) => next(AppDetailRequest(app, request))
          case None =>
            Future.successful (Ok(views.html.loginForm(msg("error.BF040")) ).withNewSession)
        }
      }
    }
}