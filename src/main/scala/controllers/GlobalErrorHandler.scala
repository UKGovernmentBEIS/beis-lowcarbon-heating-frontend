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

package controllers

import javax.inject.Singleton

import play.api.http.HttpErrorHandler
import play.api.i18n.{Messages, MessagesApi}
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent._
import javax.inject.Inject

/**
  * Created by venkatamutyala on 14/02/2018.
  */

@Singleton
class GlobalErrorHandler @Inject()(msg: MessagesApi) extends HttpErrorHandler {

  def onClientError(request: RequestHeader, statusCode: Int, message: String) = {
    println("=== ERROR: In GlobalErrorHandler ERROR Request is:=== "+request.toString())
    println("=== ERROR: In GlobalErrorHandler ERROR StatusCode and message are:=== "+statusCode + "========"+ message)
    val errMsg = msg("error.BF040")
    Future.successful(Ok(views.html.loginForm(errMsg, None)))
  }

  def onServerError(request: RequestHeader, exception: Throwable) = {
    println("=== ERROR: In GlobalErrorHandler ERROR Request is:=== "+request.toString())
    println("=== ERROR: In GlobalErrorHandler ERROR Exception is:=== "+exception)
    val errMsg = msg("error.BF040")
    Future.successful(Ok(views.html.loginForm(errMsg, None)))
  }
}
