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

import javax.inject.Inject

import eu.timepit.refined.auto._
import actions.{AccessResourceAction, AppSectionAction}
import config.Config
import forms.{Field, FileUploadItem}
import forms.validation.CostItem
import models._
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.{Action, Controller}
import services.{ApplicationFormOps, ApplicationOps, OpportunityOps}

import scala.concurrent.{ExecutionContext, Future}

class ApplicationPreviewController @Inject()(
                                              actionHandler: ActionHandler,
                                              applications: ApplicationOps,
                                              appForms: ApplicationFormOps,
                                              opps: OpportunityOps,
                                              AppSectionAction: AppSectionAction,
                                              AccessResourceAction: AccessResourceAction
                                            )(implicit ec: ExecutionContext)
  extends Controller with SessionUser{

  implicit val ciReads = Json.reads[CostItem]
  implicit val cifReads = Json.reads[FileUploadItem]

  def previewSection(id: ApplicationId, sectionNumber: AppSectionNumber) = AppSectionAction(id, sectionNumber).async { implicit request =>

    isUnAuthorisedAccess(id, sessionUser).flatMap {
      case false =>
            val (backLink, editLink) = request.appSection.section.map(_.isComplete) match {
              case Some(true) =>
                (controllers.routes.ApplicationController.show(request.appSection.id, None).url,
                  Some(controllers.routes.ApplicationController.resetAndEditSection(request.appSection.id, request.appSection.sectionNumber).url))
              case _ =>
                (controllers.routes.ApplicationController.editSectionForm(request.appSection.id, request.appSection.sectionNumber).url, None)
            }
            val answers = request.appSection.section.map { s => s.answers }.getOrElse(JsObject(List.empty))

            request.appSection.formSection.sectionType match {
              case SectionTypeForm | SimpleTypeForm  | RowForm  | TableForm | DynamicTableForm =>
                Future.successful (renderSectionPreview(request.appSection, request.appSection.formSection.fields, answers, backLink, editLink, sessionUser))
              case SectionTypeCostList =>
                val costItems = request.appSection.section.flatMap(s => (s.answers \ "items").validate[List[CostItem]].asOpt).getOrElse(List.empty)
                Future.successful (renderListPreview(request.appSection, costItems, answers, backLink, editLink, sessionUser))
              case SectionTypeFileList =>
                val fileItems = request.appSection.section.flatMap(s => (s.answers \ "items").validate[List[FileUploadItem]].asOpt).getOrElse(List.empty)
                Future.successful (renderFileListPreview(request.appSection, request.appSection.formSection.fields, answers, backLink, editLink, sessionUser))
            }
      case true => Future.successful (Ok(views.html.loginForm("Authorisation required") ).withNewSession)
    }
  }

  def renderSectionPreview(app: ApplicationSectionDetail, fields: Seq[Field], answers: JsObject, backLink: String, editLink: Option[String], userId: String) = {
    implicit val sessionUser = userId
    Ok(views.html.sectionPreview(app, fields, answers, backLink, editLink))
  }

  def renderListPreview(app: ApplicationSectionDetail, items: Seq[CostItem], answers: JsObject, backLink: String, editLink: Option[String], userId: String) = {
    implicit val sessionUser = userId
    Ok(views.html.listSectionPreview(app, items, answers, backLink, editLink))
  }

  def renderFileListPreview(app: ApplicationSectionDetail, fields: Seq[Field], answers: JsObject, backLink: String, editLink: Option[String], userId: String) = {
    implicit val sessionUser = userId
    Ok(views.html.sectionPreview(app, fields, answers, backLink, editLink))
  }

  def getFieldMap(form: ApplicationForm): Map[AppSectionNumber, Seq[Field]] = {
    Map(form.sections.map(sec => sec.sectionNumber -> sec.fields): _*)
  }

  def applicationPreview(id: ApplicationId) = Action.async { implicit request =>
    isUnAuthorisedAccess(id, sessionUser).flatMap {
      case false =>
        gatherApplicationDetails(id).map {
          case Some(app) =>
            val title = app.sections.find(_.sectionNumber == 1).flatMap(s => (s.answers \ "title").validate[String].asOpt)
            Ok(views.html.applicationPreview(app, app.sections.sortBy(_.sectionNumber), title, getFieldMap(app.applicationForm), actionHandler.guidanceDocURL))

          case _ => NotFound
        }
      case true => Future.successful (Ok(views.html.loginForm("Authorisation required") ).withNewSession)
    }
  }

  def applicationSimplePreview(id: ApplicationId, contentType: String) = Action.async { implicit request =>

    val mp = request.queryString
    val token =  actionHandler.getValueFromRequest("token", mp )
    val appFrontEndUrl = Config.config.business.appFrontEndUrl

      isUnAuthorisedAccess(id, sessionUser).flatMap {
        case false =>

          gatherApplicationDetails(id).map {
              case Some(app) =>

                val title = app.sections.find(_.sectionNumber == 1).flatMap(s => (s.answers \ "title").validate[String].asOpt)
                contentType match {
                  case "pdf" =>

                    Ok(views.html.applicationSimplePreviewPDF(app, app.sections.sortBy(_.sectionNumber), title, getFieldMap(app.applicationForm),
                      token, contentType, appFrontEndUrl, Option(sessionUser)))
                  case _ =>
                    Ok(views.html.applicationSimplePreview(app, app.sections.sortBy(_.sectionNumber), title, getFieldMap(app.applicationForm),
                      token, contentType, Option(sessionUser)))
                }
               case _ => NotFound
          }
        case true =>

          actionHandler.isAuthTokenValid(token, Some(id)) match {
              case true =>

                gatherApplicationDetails(id).map {
                      case Some(app) =>
                        val title = app.sections.find(_.sectionNumber == 1).flatMap(s => (s.answers \ "title").validate[String].asOpt)
                          contentType match {
                            case "pdf" =>
                              Ok(views.html.applicationSimplePreviewPDF(app, app.sections.sortBy(_.sectionNumber), title,
                                  getFieldMap(app.applicationForm), token, contentType, appFrontEndUrl, None))
                                case _ =>
                                  Ok(views.html.applicationSimplePreview(app, app.sections.sortBy(_.sectionNumber), title, getFieldMap(app.applicationForm),
                                    token, contentType, Option(sessionUser)))
                              }
                        case None =>
                        Ok(views.html.loginForm("Authorisation required") ).withNewSession
                  }
              case false =>
                Future.successful (Ok(views.html.loginForm("Authorisation required") ).withNewSession)
          }
    }
  }


  def isUnAuthorisedAccess(id:ApplicationId, sUser: String) = {

    applications.byId(id).flatMap {
      case Some(app) =>

        if(!app.userId.userId.equals(sUser))
          Future(true)
        else
          Future(false)
      case None => Future(true)
    }
  }


  def gatherApplicationDetails(id: ApplicationId): Future[Option[ApplicationDetail]] = applications.detail(id)


}


