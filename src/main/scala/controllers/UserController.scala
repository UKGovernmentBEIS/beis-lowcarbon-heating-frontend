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
import forms.validation.{EmailValidator, FieldError}
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

//import play.api.data.validation.Validation
import play.api.data.validation._

import cats.data.ValidatedNel
import cats.syntax.cartesian._
import cats.syntax.validated._
import forms.validation.FieldValidator.Normalised


/********************************************************************************
  This file is for temporary Login till any Security component is deployed.
  This file also for Activity samples.
  Please donot use this login file. i.e dont use http://localhost:9000/login
  Use only http://localhost:9000
 *********************************************************************************/

class UserController @Inject()(users: UserOps)(implicit ec: ExecutionContext)
  extends Controller with ApplicationResults /*with Constraint[String]*/ {

  implicit val loginFormWrites = Json.writes[LoginForm]
  implicit val regnFormWrites = Json.writes[RegistrationForm]


  val loginform:Form[LoginForm] = Form(
    mapping(
      "name" -> text,
      "password" -> text
    ) (LoginForm.apply)(LoginForm.unapply) verifying ("Invalid email or password", result => result match {
      case loginForm => checkNotNull(loginForm.name, loginForm.password)
    })
  )

  val registrationform:Form[RegistrationForm] = Form(
    mapping(
      "name" -> text,
      "confirmpassword" -> text,
      "password" -> text,
      "email" -> text
    ) (RegistrationForm.apply)(RegistrationForm.unapply) verifying ("Invalid email or password", result => result match {
      case registrationForm => checkNotNull(registrationForm.name, registrationForm.password)
    })
  )

  val registrationform1:Form[RegistrationForm] = Form(
    mapping(
      "name" -> text,
      "password" -> text,
      "confirmpassword" -> text,
      "email" -> text
    ) (RegistrationForm.apply)(RegistrationForm.unapply) verifying ("Invalid email or password", result => result match {
      case registrationForm =>
        false
     })
  )

  val forgotpasswordform:Form[ForgotPasswordForm] = Form(
    mapping(
      "email" -> text
    ) (ForgotPasswordForm.apply)(ForgotPasswordForm.unapply) verifying ("Invalid email", result => result match {
      case forgotpasswordform => checkEmailNotNull(forgotpasswordform.email.replaceAll("\\s+", ""))
    })
  )

  def checkNotNull(username: String, password: String) =
    (!StringUtils.isEmpty(username) && !StringUtils.isEmpty(password))

  def checkEmailNotNull(email: String) =
    (!StringUtils.isEmpty(email))

  def checkPassword(password: String) = {
      (password.length > 8 )
  }

  def loginForm = Action{
    Ok(views.html.loginForm("", loginform))
  }

  def basicAuth(pswd: String) = {
    new String(Base64.getEncoder.encode((pswd).getBytes))
  }

  def confirmPasswordCheck(password:String,confirmpassword:String): List[FieldError] = {

    password.equals(confirmpassword) match {
      case false =>   List(FieldError("password", "Password doesn't match"))
      case true => List()
    }
  }

  def registrationSubmit =  Action.async(JsonForm.parser)  { implicit request =>

    val jsObj = Json.toJson(request.body.values).as[JsObject] + ("id" -> Json.toJson(0))
    val password = (request.body.values \ "password").validate[String].getOrElse("NA")
    val confirmpassword = (request.body.values \ "confirmpassword").validate[String].getOrElse("NA")
    val email = (request.body.values \ "email").validate[String].getOrElse("NA")

    val emailvalidator = EmailValidator(Option("email")).validate("email", email).fold(_.toList, _ => List())
    //val emailvalidator = EmailValidator(Option("email")).validate("email", email)
    val errors = confirmPasswordCheck(password, confirmpassword) ++ emailvalidator

    errors.isEmpty match {
      case true =>
          users.register(jsObj - "confirmpassword").flatMap{
              case Some(msg) => {
                /** TODO *************
                  * need to get the exception TYPE from backend for the exceptions and show them
                  * or need to get the error number to select the error text from any resource bundle or properties
                  */
                val dbUniqueKeyError = "duplicate key value violates unique constraint"
                if(msg.indexOf(dbUniqueKeyError) != -1) {
                  val username = (request.body.values \ "name").validate[String].getOrElse("NA")
                  val errorMsg = s"'$username' already exists. Please choose other name"
                  Future.successful(Ok(views.html.registrationForm(registrationform, List())))
                }
                else
                  Future.successful(Ok(views.html.loginForm("", loginform)))
              }
              case None =>
                Future.successful(Ok(views.html.loginForm("Login is incorrect. Please add correct details", loginform)))
          }
      case false =>
        val errMsg = "The passwords entered do not match. Please enter them again"
        Future.successful(Ok(views.html.registrationForm(registrationform, errors)))
    }
  }

  def forgotPasswordSubmit = Action.async(JsonForm.parser)  { implicit request =>
    val email = (request.body.values \ "email").validate[String].getOrElse("NA")
    users.forgotpassword(Json.toJson(request.body.values).as[JsObject]).flatMap{
      case Some(msg) => {
        /** TODO *************
          * need to get the exception TYPE from backend for the exceptions and show them
          * or need to get the error number to select the error text from any resource bundle or properties
          */
        val dbError = "empty.head"
        if (msg.indexOf(dbError) != -1) {
          val username = (request.body.values \ "name").validate[String].getOrElse("NA")
          val errorMsg = "Details not found. Please add correct details"
          Future.successful(Ok(views.html.forgotPasswordForm(errorMsg, forgotpasswordform)))
        }
        else
          Future.successful(Ok(views.html.forgotPasswordConfirm(email)))
      }
      case None =>
        Future.successful(Ok(views.html.forgotPasswordForm("Details are incorrect. Please add correct details", forgotpasswordform)))
    }
  }

  def loginFormSubmit = Action.async(JsonForm.parser)  { implicit request =>

  val username = (request.body.values \ "name").validate[String].getOrElse("NA")
    users.login(Json.toJson(request.body.values).as[JsObject]).flatMap{
      case Some(msg) =>
        Future.successful(Redirect(routes.DashBoardController.applicantDashBoard()).withSession(Security.username -> username))
      case None =>
        Future.successful(Ok(views.html.loginForm("Login is incorrect. Please add correct details", loginform)))
    }
  }

  def logOut = Action{
    Ok(views.html.loginForm("", loginform)).withNewSession
  }

  def registrationForm = Action{
    //Ok(views.html.registrationForm("", registrationform))
    Ok(views.html.registrationForm(registrationform, List()))
  }

  def forgotPasswordForm = Action{
    Ok(views.html.forgotPasswordForm("", forgotpasswordform))
  }


  def start(pid:Int, processEngine: ProcessEngineWrapper){
    processEngine.engine.getRuntimeService().startProcessInstanceByKey("logging-test")
  }

   /* def registrationSubmit__ = Action { implicit request =>

    registrationform.bindFromRequest.fold(
      errors => {
        Ok(views.html.registrationForm("testtest", registrationform))
      },
      regnForm=> {
            users.register(Json.toJson(regnForm).as[JsObject] - "confirmpassword").flatMap{
              case Some(msg) => {
                /** TODO *************
                  * need to get the exception TYPE from backend for the exceptions and show them
                  * or need to get the error number to select the error text from any resource bundle or properties
                  */
                val dbUniqueKeyError = "duplicate key value violates unique constraint"
                if(msg.indexOf(dbUniqueKeyError) != -1) {
                  val errorMsg = s"'${regnForm.name}' already exists. Please choose other name"
                  Future.successful(Ok(views.html.registrationForm(errorMsg, registrationform)))
                }
                else
                  Future.successful(Ok(views.html.loginForm("", loginform)))
              }
              case None =>
                Future.successful(Ok(views.html.loginForm("Login is incorrect. Please add correct details", loginform)))
            }
        Ok(views.html.registrationForm("testtest", registrationform))

      }
    )
  }*/

  /*
    def loginFormSubmit_ = Action { implicit request =>

      loginform.bindFromRequest.fold(
        errors => {
          Ok(views.html.loginForm("error", loginform))
        },
        user=> {

          //TODO:- Here the Roles come into place and Users belong to Group or Role
          implicit val userIdInSession = user.name
          if(user.name.equals("applicant1") || user.name.equals("applicant2") || user.name.equals("applicant3") || user.name.equals("applicant4"))
            Redirect(routes.DashBoardController.applicantDashBoard()).withSession(Security.username -> user.name)
          else if(user.name.equals("manager") || user.name.equals("portfoliomanager"))
            Redirect(routes.DashBoardController.staffDashBoard()).withSession(Security.username -> user.name)
          //Redirect(manage.routes.OpportunityController.showNewOpportunityForm()).withSession(Security.username -> user.name)
          else
            Redirect(routes.OpportunityController.showOpportunities()).withSession(Security.username -> user.name)
        }
      )
    }

    def registrationSubmit_ = Action { implicit request =>

      registrationform.bindFromRequest.fold(
        errors => {
          Ok(views.html.registrationForm("error", registrationform))
        },
        regnForm=> {
          users.register(Json.toJson(regnForm).as[JsObject] - "confirmpassword").flatMap{
            case Some(msg) => {
              /** TODO *************
                * need to get the exception TYPE from backend for the exceptions and show them
                * or need to get the error number to select the error text from any resource bundle or properties
                */
              val dbUniqueKeyError = "duplicate key value violates unique constraint"
              if(msg.indexOf(dbUniqueKeyError) != -1) {
                val errorMsg = s"'${regnForm.name}' already exists. Please choose other name"
                Future.successful(Ok(views.html.registrationForm(errorMsg, registrationform)))
              }
              else
                Future.successful(Ok(views.html.loginForm("", loginform)))
            }
            case None =>
              Future.successful(Ok(views.html.loginForm("Login is incorrect. Please add correct details", loginform)))
          }
          //Redirect(Ok(views.html.registrationForm("testtest", registrationform)))
          Redirect(routes.UserController.registrationForm())


        }
      )
    }

    def confirmPasswordCheck(password:String,confirmpassword:String): ValidatedNel[FieldError, String] = {
    password.equals(confirmpassword) match {
      case true =>    FieldError("password", "Password doesnt match").invalidNel
      case false => "".validNel
    }
  }
*/

 }

case class LoginForm(name: String, password: String)
case class RegistrationForm(name: String, password: String, confirmpassword: String, email: String)
case class RegistrationFormValues(name: Option[String]= None, password: Option[String]= None, confirmpassword: Option[String]= None, email: String)
case class ForgotPasswordForm(email: String)
