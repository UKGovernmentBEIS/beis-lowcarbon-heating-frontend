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

package filters

import play.api.mvc._
import play.api.mvc.Results.Redirect
import play.api.mvc.Results._
import play.api.mvc._
import javax.inject.Inject

import scala.concurrent.{ExecutionContext, Future}
import akka.stream.Materializer
import config.Config
import controllers.SessionUser
import org.apache.commons.lang3.StringUtils

import util.control.Breaks._
import scala.util.control._
import play.api.i18n.MessagesApi
import play.api.i18n.Messages.Implicits._

/**
  * Created by venkatamutyala on 18/09/2017.
  */

class AuthoriseFilter @Inject()(implicit val mat: Materializer, ec: ExecutionContext, msg: MessagesApi) extends Filter {

  import play.api.mvc.Results._

  override def apply(nextCall: (RequestHeader) => Future[Result])( rh: RequestHeader): Future[Result] = {

    if (rh.uri.startsWith("/assets") || rh.uri.startsWith("/resetpassword")|| rh.uri.startsWith("/opportunity")
      || isSessionExcluded(rh) /* || rh.uri.startsWith("/pdf")*/) {
      nextCall(rh)
    }
    else {

        isSessionTimedOut(rh.session.get("sessionTime").getOrElse(System.currentTimeMillis.toString).toLong) match {
          case true =>
            println("Error in AuthFilter - no Session ")
            Future.successful (Ok (views.html.loginForm (msg("error.BF039")) ).withNewSession)
          case false => {
          val isOppClosed = rh.session.get("isOppClosed").getOrElse("false")

            (isOppClosed.toBoolean && rh.uri.startsWith("/application/")) match{
                    case true => Future.successful (Ok (views.html.loginForm (msg("error.BF040")) ).withNewSession)
                    case false =>
                        rh.session.get ("username").map {
                        user =>
                          nextCall (rh).flatMap {
                            a =>
                              Future (a.withSession (
                                (Security.username -> rh.session.get ("username").get),
                                ("sessionTime" -> System.currentTimeMillis.toString),
                                ("isOppClosed" -> isOppClosed)
                              ))
                          }
                      }.getOrElse {
                          if(StringUtils.isNotEmpty(rh.getQueryString("token").getOrElse(""))) {
                            nextCall (rh)
                          }else{
                          println("Error in AuthFilter - no Token - no Login ")
                          Future.successful (Ok (views.html.loginForm (msg("error.BF040")) ) )
                        }
                      }
                  }


          }
        }
    }
  }

  def isSessionTimedOut(sessionTime: Long):Boolean = {
    val sessionTimeout = Config.config.login.sessionTimeout
    val currentTime = System.currentTimeMillis
    (currentTime - sessionTime) > sessionTimeout
  }

  def isSessionExcluded(rh: RequestHeader):Boolean = {
    val Outer = new Breaks
    Outer.breakable {
      Config.config.login.excludeSession.split(",").map { endpoint =>
        if (rh.path.equals(endpoint)) {
          return true
          break
        }
      }
    }
    false
  }
}