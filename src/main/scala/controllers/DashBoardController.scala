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

import java.io.File
import javax.inject.Inject

import actions.{AppDetailAction, AppSectionAction, OppSectionAction, OpportunityAction}
import config.Config
import eu.timepit.refined.auto._
import forms.validation._
import forms.{FileList, FileUploadItem, TextField}
import models.{LongId, _}
import org.joda.time.LocalDateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.Files.TemporaryFile
import play.api.libs.json._
import play.api.mvc.{Action, Controller, MultipartFormData, Result}
import services.{ApplicationFormOps, ApplicationOps, MessageBoardOps, OpportunityOps}

import scala.concurrent
import scala.concurrent.{ExecutionContext, Future}

import scala.util.{Failure, Success}

class DashBoardController @Inject()(   applications: ApplicationOps,
                                       actionHandler: ActionHandler,
                                       opps: OpportunityOps,
                                       apps: ApplicationOps,
                                       appforms: ApplicationFormOps,
                                       OppSectionAction: OppSectionAction,
                                       msgs: MessageBoardOps
                                     )(implicit ec: ExecutionContext)
  extends Controller with ApplicationResults with SessionUser{

  def applicantDashBoard = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    for(
        appsSeq <- applications.getApplicationsByUserId(UserId(userId)).map{
        case apps: Seq[Application] => apps
        case _ => Seq()
        };
        oppsSeq <- opps.getOpenOpportunitySummaries.map {
        case ops: Seq[Opportunity] => ops
        case _ => Seq()
        }/*;
        msgSeq <- msgs.byUserId(UserId(userId)).map {
        case msgs: Seq[Message] => msgs
        case _ => Seq()
        }*/
    )yield(
      Ok(views.html.showApplicantDashBoard(appsSeq, oppsSeq/*, msgSeq*/)))
  }


  def applicantDashBoard_ = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    for(
      appsSeq <- applications.getApplicationsByUserId(UserId(userId)).map{
        case apps: Seq[Application] => apps
        case _ => Seq()
      };
      oppsSeq <- opps.getOpenOpportunitySummaries.map {
        case ops: Seq[Opportunity] => ops
        case _ => Seq()
      }/*;
        msgSeq <- msgs.byUserId(UserId(userId)).map {
        case msgs: Seq[Message] => msgs
        case _ => Seq()
        }*/
    )yield(
      Ok(views.html.showApplicantDashBoard(appsSeq, oppsSeq/*, msgSeq*/)))
  }


//------------
//  def applicantDashBoard = Action.async { implicit request =>
//    val userId = request.session.get("username").getOrElse("Unauthorised User")
//
//    for(
//      appsSeq <- applications.getApplicationsByUserId(UserId(userId)).map{
//        case apps: Seq[Application] => apps
//        case _ => Seq()
//      };
//      oppsSeq <- opps.getOpenOpportunitySummaries.map {
//        case ops  =>
//          ops.map(o=> (o,
//            showOpportunitySection(o.id, OppSectionNumber(1), userId).onComplete {
//            case Success(value) => value
//            case Failure(e) => None
//          }.asInstanceOf[Option[Application]]
//
//
//          )).toSeq
//        case _ => Seq[ (Opportunity, Option[Application]) ]()
//      }/*;
//        msgSeq <- msgs.byUserId(UserId(userId)).map {
//        case msgs: Seq[Message] => msgs
//        case _ => Seq()
//        }*/
//    )yield(
//      //oppsSeq.map(s=> s._2.asInstanceOf[Option[Application].get.])
//      Ok(views.html.showApplicantDashBoard(oppsSeq)))
//  }
//
//  def showOpportunitySection(id: OpportunityId, sectionNum: OppSectionNumber, userId: String):Future[Option[Application]] =  {
//    appforms.byOpportunityId(id).flatMap {
//      case Some(appform) => apps.byFormId(appform.id, UserId(userId)).flatMap{
//        case app:Future[Option[Application]] => Future(app)
//          //Future.successful(Ok(views.html.showOpportunity(appform, app, request.opportunity, request.section, userId, Option(actionHandler.guidanceDocURL))))
//        // None => Future.successful(NotFound)
//      }
//      case None => Future(None)
//    }
//  }
//
//
//  def showOpportunitySection_(id: OpportunityId, sectionNum: OppSectionNumber) = OppSectionAction(id, sectionNum).async { implicit request =>
//    val userId = request.session.get("username").getOrElse("Unauthorised User")
//    //TODO:- need to merge these 2 Database calls to one
//    appforms.byOpportunityId(id).flatMap {
//      case Some(appform) => apps.byFormId(appform.id, UserId(userId)).flatMap{
//        case app: Option[Application] =>
//          Future.successful(Ok(views.html.showOpportunity(appform, app, request.opportunity, request.section, userId, Option(actionHandler.guidanceDocURL))))
//        // None => Future.successful(NotFound)
//      }
//      case None => Future.successful(NotFound)
//    }
//  }

  //------------


  def staffDashBoard = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    for(

      oppsSeq <- opps.getOpenOpportunitySummaries.map {
          case ops: Seq[Opportunity] => //ops
        {

          ops.filter( op =>
            appforms.byOpportunityId(op.id).map(  s=> s.getOrElse(ApplicationForm(null,null,null)).sections.head.sectionType.name)
              == "simpleform")
          ops
        }

          /*oppsSeq <- opps.getOpenOpportunitySummaries.map {
          case ops: Seq[Opportunity] => //ops
          {

            ops.filter( op =>
              appforms.byOpportunityId(op.id).map(  s=> s.getOrElse(ApplicationForm(null,null,null)).sections.head.sectionType.name)
                == "simpleform")

          }*/
        case _ => Seq()
      }/*;
      msgSeq <- msgs.byUserId(UserId(userId)).map {
        case msgs: Seq[Message] => msgs
        case _ => Seq()
      }*/
    )yield(
      Ok(views.html.showStaffDashBoard(/*appsSeq, */oppsSeq/*, msgSeq*/)))
  }
}
