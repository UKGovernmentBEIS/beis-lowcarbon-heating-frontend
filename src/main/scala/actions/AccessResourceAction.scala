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

import models.{ApplicationId}
import play.api.mvc.Results._
import play.api.mvc._
import services.ApplicationOps

import scala.concurrent.{ExecutionContext, Future}
import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import javax.inject.Inject

import config.Config
import play.api.libs.json.Json
import play.api.mvc._
import services._
import play.api.libs.json.Json
import play.api.mvc._
import services.JWTOps
import models.User

case class AccessResourceRequest[A](isAllowedByRole: Boolean, isAllowedByUser: Boolean, isOpportunityClosed: Boolean,
                                    request: Request[A]) extends WrappedRequest[A](request)

class AccessResourceAction @Inject()(applications: ApplicationOps, jwt: JWTOps)(implicit ec: ExecutionContext)  {

  implicit val messages = Messages

  def apply(id: ApplicationId): ActionBuilder[AccessResourceRequest] =

    new ActionBuilder[AccessResourceRequest] {

      override def invokeBlock[A](request: Request[A], next: (AccessResourceRequest[A]) => Future[Result]): Future[Result] = {
        //TODO:-SOMEHOW THIS CDDE IS GIVING ERROR
        /* applications.byId(id).flatMap {
          case Some(app) =>
            val userId = "cc"
            if(app.userId.userId.equals(userId))
              next(AccessResourceRequest(true, true, true, request))
            else
              Future.successful (Ok(views.html.loginForm("Authorisation required") ).withNewSession)
          case None =>
            next(AccessResourceRequest(true, true, true, request))
        }*/
        next(AccessResourceRequest(true, true, true, request))
      }
    }
}


/*def apply(id: ApplicationId): ActionBuilder[AccessResourceRequest] =

    new ActionBuilder[AccessResourceRequest] {

      override def invokeBlock[A](request: Request[A], next: (AccessResourceRequest[A]) => Future[Result]): Future[Result] = {
        //val userId = request.session.get("username").getOrElse("Unauthorised User")
        val userId = "cc"

        /*
        UnAuhorised access checks:-
          1. AUTHORISED :- Requesting user belong to a Authorised Process management group (eg. Policyadmin)
          2. UN-AUTHORISED :- Requested resource is not available when end_date of Opportunity is expired
          3. UN-AUTHORISED :- Requesting user is not the user of the Application
                               - this is to restrict hackers try to access others Applications
         */

        /* 1. AUTHORISED :- Requesting user belong to a Authorised Process management group (eg. Policyadmin)
               (OUT SIDE USERS REQUEST USING REST API WHO ARE OF AUTHORISED GROU)P */
//
//        val jwtSecretKey = Config.config.jwt.jwtSecretKey
//        val jwtSecretAlgo = Config.config.jwt.jwtSecretAlgo
//        val processMgmtGroup = Config.config.jwt.processMgmtGroup
//        val jwtToken = request.headers.get("jw_token").getOrElse("")
//
//        /* Using JWT JSON Web Token - REST call request contain Role information and Encripted SecretKey*/
//        val isAllowedbyRole1 = if(jwt.isValidToken(jwtToken)) {
//          jwt.decodePayload(jwtToken).fold {
//            false
//          } { payload =>
//            val userCredentials = Json.parse(payload).validate[User].get //need to add fee more CLAIMS
//            processMgmtGroup.equals(userCredentials.role.getOrElse("")) match {
//              case true => true
//              case false => false
//            }
//          }
//        } else {
//          false
//        }
////
//        /* 2. UN-AUTHORISED :- Requested resource is not available when end_date of Opportunity is expired */
//
//        val  isOpportunityClosed_ = true //TODO:- design issue. need to get the User vs List of Opportunities in session when user logs in.
//
//        /* 3. UN-AUTHORISED :- Requesting user is not the user of the Application */
//
        applications.byId(id).flatMap {
          case Some(app) =>
            println("==999====="+app.userId.userId.equals(userId) )
            if(app.userId.userId.equals(userId))
              next(AccessResourceRequest(true, true, true, request))
            else
              Future.successful (Ok(views.html.loginForm("Authorisation required") ).withNewSession)
          case None =>
            next(AccessResourceRequest(true, true, true, request))
        }

      // next(AccessResourceRequest(true, true, true, request))

      }

    }

}

*/