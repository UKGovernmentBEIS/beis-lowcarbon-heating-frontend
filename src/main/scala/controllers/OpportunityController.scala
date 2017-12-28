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
import actions.{OppSectionAction, OpportunityAction}
import config.Config
import models.{AppSectionNumber, Application, OppSectionNumber, OpportunityId, UserId}
import org.joda.time.LocalDate
import play.api.mvc.Results.NotFound
import play.api.mvc.{Action, Controller, Security}
import services.{ApplicationFormOps, ApplicationOps, OpportunityOps}

import scala.concurrent.{ExecutionContext, Future}

class OpportunityController @Inject()( actionHandler: ActionHandler,
                                       opportunities: OpportunityOps,
                                       appForms: ApplicationFormOps,
                                       apps: ApplicationOps,
                                       OpportunityAction: OpportunityAction,
                                       OppSectionAction: OppSectionAction
                                     )(implicit ec: ExecutionContext) extends Controller with SessionUser {

  def showOpportunities = Action.async { implicit request =>
    opportunities.getOpenOpportunitySummaries.map { os => Ok(views.html.showOpportunities(os)) }
  }

  def showOpportunity(id: OpportunityId, sectionNumber: Option[OppSectionNumber]) = OpportunityAction(id) { implicit request =>
    Redirect(controllers.routes.OpportunityController.showOpportunitySection(id, sectionNumber.getOrElse(OppSectionNumber(1))))
  }

  def isSessionTimedOut(sessionTime: Long):Boolean = {
    val sessionTimeout = Config.config.login.sessionTimeout
    val currentTime = System.currentTimeMillis
    (currentTime - sessionTime) > sessionTimeout
  }

  def showOpportunitySection(id: OpportunityId, sectionNum: OppSectionNumber) = OppSectionAction(id, sectionNum).async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")
    val isOppClosed =
            if(!request.opportunity.endDate.isEmpty) {
               request.opportunity.endDate.get.toDateTimeAtStartOfDay().plusHours(17) //added to make 5pm of the same day
                  .isBefore(LocalDate.now().toDateTimeAtCurrentTime) match {
                 case true => "true"
                 case false => "false"
               }
            }
            else
               "false"

    //TODO:- need to merge these 2 Database calls to one
    appForms.byOpportunityId(id).flatMap {
       case Some(appform) => apps.byFormId(appform.id, UserId(userId)).flatMap{
            case app: Option[Application] => Future.successful(Ok(views.html.showOpportunity(appform, app, request.opportunity, request.section,
              userId, isOppClosed.toBoolean, Option(actionHandler.guidanceDocURL)))
              .withSession(request.session + ("isOppClosed" -> isOppClosed)))
       }
       case None => Future.successful(NotFound)
    }
  }


  def showGuidancePage(id: OpportunityId) = Action { implicit request =>
    Ok(views.html.guidance(id))
  }

  def wip(backUrl: String) = Action {
    Ok(views.html.wip(backUrl))
  }

}



