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

import java.util
import java.util.Base64
import javax.inject.Inject

import controllers.JsonHelpers.formToJson
import org.activiti.engine.impl.persistence.entity.ProcessDefinitionEntity
import org.activiti.engine.repository.ProcessDefinition
import org.activiti.engine.task.Task
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json._
import play.api.libs.json.Json
import org.activiti.engine.{ProcessEngine, ProcessEngines}

import scala.util.{Failure, Success}
import scala.concurrent.{ExecutionContext, Future}
import org.apache.commons.lang3.StringUtils
import services.RestService.JsonParseException
import services.UserOps

/********************************************************************************
  This file is for temporary Login till any Security component is deployed.
  This file also for Activity samples.
  Please donot use this login file. i.e dont use http://localhost:9000/login
  Use only http://localhost:9000
 *********************************************************************************/

class UserController @Inject()(users: UserOps)(implicit ec: ExecutionContext)
  extends Controller with ApplicationResults {

  implicit val loginFormWrites = Json.writes[LoginForm]
  implicit val regnFormWrites = Json.writes[RegistrationForm]


  val loginform:Form[LoginForm] = Form(
    mapping(
      "name" -> text,
      "password" -> text
    ) (LoginForm.apply)(LoginForm.unapply) verifying ("Invalid email or password", result => result match {
      case loginForm => {
        checkNotNull(loginForm.name, loginForm.password)
      }
    })
  )

  val registrationform:Form[RegistrationForm] = Form(
    mapping(
      "name" -> text,
      "password" -> text,
      "email" -> text
    ) (RegistrationForm.apply)(RegistrationForm.unapply) verifying ("Invalid email or password", result => result match {
      case registrationform => checkNotNull(registrationform.name.replaceAll("\\s+", ""),
        registrationform.password.replaceAll("\\s+", ""))
    })
  )

  def checkNotNull(username: String, password: String) =
     (!StringUtils.isEmpty(username) && !StringUtils.isEmpty(password))


  def checkPassword(password: String) = {
      (password.length > 8 )
  }

  def loginForm = Action{
    Ok(views.html.loginForm("", loginform))
  }

  def loginFormSubmit = Action.async(JsonForm.parser)  { implicit request =>

        //TODO:- Here the Roles come into place and Users belong to Group or Role
    val username = (request.body.values \ "name").validate[String].getOrElse("NA")

        users.login(Json.toJson(request.body.values).as[JsObject]).flatMap{
                    case Some(msg) =>
                      Future.successful(Redirect(routes.DashBoardController.applicantDashBoard()).withSession(Security.username -> username))

                    case None =>
                      Future.successful(Ok(views.html.loginForm("Login is incorrect. Please add correct details", loginform)))
                  }
  }

  def forgotpasswordSubmit = Action.async(JsonForm.parser)  { implicit request =>
    ???
  }



  def basicAuth(pswd: String) = {
    new String(Base64.getEncoder.encode((pswd).getBytes))
  }

  def registrationSubmit =  Action.async(JsonForm.parser)  { implicit request =>

    val jsObj = Json.toJson(request.body.values).as[JsObject] + ("id" -> Json.toJson(0))

      users.register(jsObj).flatMap{
      case Some(msg) => {

        /** TODO *************
          * need to get the exception TYPE from backend for the exceptions and show them
          * or need to get the error number to select the error text from any resource bundle or properties
          */

        if(msg.indexOf("duplicate key value violates unique constraint") != -1) {
          val username = (request.body.values \ "name").validate[String].getOrElse("NA")
          val errorMsg = s"'$username' already exists. Please choose other name"
          Future.successful(Ok(views.html.registrationForm(errorMsg, registrationform)))
        }
        else
         // Future.successful(Redirect(routes.DashBoardController.applicantDashBoard()).withSession(Security.username -> ""))
          Future.successful(Ok(views.html.loginForm("", loginform)))

      }
      case None =>
        Future.successful(Ok(views.html.loginForm("Login is incorrect. Please add correct details", loginform)))
    }
  }


  def logOut = Action{
    Ok(views.html.loginForm("", loginform)).withNewSession
  }

  def registrationForm = Action{
    Ok(views.html.registrationForm("", registrationform))
  }


  def start(pid:Int, processEngine: ProcessEngineWrapper){
    processEngine.engine.getRuntimeService().startProcessInstanceByKey("logging-test")
  }

}

case class LoginForm(name: String, password: String)
case class RegistrationForm(name: String, password: String, email: String)
