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

import config.Config
import forms.{DynamicTableItem, FileUploadItem, TableItem}
import forms.validation.CostItem
import models._
import org.joda.time.DateTime
import play.api.libs.json.{JsObject, _}
import play.api.mvc.Result
import play.api.mvc.Results._
import services.{ApplicationFormOps, ApplicationOps, BusinessProcessOps, JWTOps, OpportunityOps}

import scala.concurrent.{ExecutionContext, Future}

class ActionHandler @Inject()(applications: ApplicationOps, applicationForms: ApplicationFormOps, opportunities: OpportunityOps,
                              processes: BusinessProcessOps, jwt: JWTOps)(implicit ec: ExecutionContext)
  extends ApplicationResults with SessionUser {

  import ApplicationData._
  import FieldCheckHelpers._

  implicit val tableItemF = Json.format[DynamicTableItem]


  def doSave(app: ApplicationSectionDetail, fieldValues: JsObject, userId: String): Future[Result] = {
    app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm => {
        if (JsonHelpers.allFieldsEmpty(fieldValues)) applications.deleteSection(app.id, app.sectionNumber)
        else applications.saveSection(app.id, app.sectionNumber, fieldValues)
      }.map(_ => redirectToOverview(app.id, Some(app.sectionNumber.num.value)))
      case SectionTypeCostList => Future.successful(redirectToOverview(app.id, Some(app.sectionNumber.num.value)))
      case SectionTypeFileList => Future.successful(redirectToOverview(app.id, Some(app.sectionNumber.num.value)))
      case DynamicTableForm => Future.successful(redirectToOverview(app.id, Some(app.sectionNumber.num.value)))
    }
  }

  def doComplete(app: ApplicationSectionDetail, fieldValues: JsObject, userId: String): Future[Result] = {
    val answers = app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm  => fieldValues
      // Instead of using the values that were passed in from the form we'll use the values that
      // have already been saved against the item list, since these were created by the add-item
      // form.
      case SectionTypeCostList => app.section.map(_.answers).getOrElse(JsObject(Seq()))
      case SectionTypeFileList => app.section.map(_.answers).getOrElse(JsObject(Seq()))
      case DynamicTableForm => app.section.map(_.answers).getOrElse(JsObject(Seq()))
    }
    applications.completeSection(app.id, app.sectionNumber, answers).map {
      case Nil => redirectToOverview(app.id, Some(app.sectionNumber.num.value))
      case errs => {
        redisplaySectionForm(app, answers, errs, userId)
      }
    }
  }

  def doSaveItem(app: ApplicationSectionDetail, fieldValues: JsObject, userId: String): Future[Result] = {

    JsonHelpers.allFieldsEmpty(fieldValues) match {
      case true => applications.deleteSection(app.id, app.sectionNumber).map(_ => redirectToOverview(app.id))
      //  applications.saveFileItem(id, sectionNumber, JsObject(Seq("item" -> Json.toJson(fileUploadItem)))).flatMap {

          case false => applications.saveItem(app.id, app.sectionNumber, fieldValues).flatMap {
        case Nil => Future.successful(redirectToOverview(app.id))
        case errs => Future.successful(redisplaySectionForm(app, fieldValues, errs, userId))
      }
    }
  }


  def doSaveDynamicTDItem(app: ApplicationSectionDetail, fieldValues: JsObject, userId: String): Future[Result] = {
    val tddata = fieldValues.fields.head._2.toString().replaceAll("^\"|\"$\"[\"]", "")
    val tdatatmp  = tddata.replaceAll("""","""","*column*")//.replaceAll("""\\""", "")
    val t = tdatatmp.substring(2, tdatatmp.length-2).split("\\*column\\*")
    val tableItem:DynamicTableItem = DynamicTableItem(t)
    JsonHelpers.allFieldsEmpty(fieldValues) match {
      case true => applications.deleteSection(app.id, app.sectionNumber).map(_ => redirectToOverview(app.id))
      case false =>
        applications.saveItem(app.id, app.sectionNumber, JsObject(Seq("item" -> Json.toJson(tableItem))) ).flatMap {
          case Nil => Future.successful(redirectToSectionForm(app.id, app.sectionNumber))
          case errs => Future.successful(redisplaySectionForm(app, fieldValues, errs, userId))
        }
    }
  }


  def doSaveFileItem(app: ApplicationSectionDetail, fieldValues: JsObject): Future[Result] = {

    JsonHelpers.allFieldsEmpty(fieldValues) match {
      case true => applications.deleteSection(app.id, app.sectionNumber).map(_ => redirectToOverview(app.id))
      case false => applications.saveFileItem(app.id, app.sectionNumber, fieldValues).flatMap {
        case itemnumber => Future.successful(redirectToOverview(app.id))
        //case errs => Future.successful(redisplaySectionForm(app, fieldValues, errs))
      }
    }
  }

  def doPreview(app: ApplicationSectionDetail, fieldValues: JsObject, userId: String): Future[Result] = {
    app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm =>
        val errs = check(fieldValues, previewChecksFor(app.formSection))
        if (errs.isEmpty) applications.saveSection(app.id, app.sectionNumber, fieldValues).map(_ => redirectToPreview(app.id, app.sectionNumber))
        else Future.successful(redisplaySectionForm(app, fieldValues, errs, userId))

      case SectionTypeCostList => Future.successful(redirectToPreview(app.id, app.sectionNumber))
      case _ => Future.successful(NotFound)
    }
  }

  def doSubmit(id: ApplicationId, applicationDetail: ApplicationDetail, userId: UserId): Future[Option[SubmittedApplicationRef]] = {
    /** Create ProcessDefinition object and activate  **/

    val bpmreqd = Config.config.bpm.bpmreqd.toBoolean

    bpmreqd match {
      case true =>
            val pdId = ProcessDefinitionId(Config.config.bpm.procdefId)
            val pd = ProcessDefinition(pdId, BusinessKey("businessKey"+ pdId), false, processVariables(applicationDetail, userId))

            /* 2 type of submits to Activiti
                1)Submit First time by Applicant:- Create new Process Instance AND Update the BEIS forms Applicationn status to Submit
                2)Submit for 'Request for more Info':- Update existing ProcessInstance AND Update the BEIS forms Applicationn status to Submit
            */
            applicationDetail.appStatus.appStatus match {

              case "In progress" =>  {  /* Fresh Application , so activate the BPM Process*/

                /** Save Application only if Process database is updated without errors */
                if(bpmreqd) { ///Is there any need of Back office processing?

                  processes.activateProcess(pdId, pd).flatMap {
                    case Some(procInstId) => {
                      /** Update Appplication record with Submit status **/
                      applications.submit(id)
                    }
                    case _ => Future.successful(None)
                  }
                }else
                  applications.submit(id)
              }
            case _ => { /* Already submitted Application, and came back for 'Request for more info' */

              /* Update the existing process instance - get ExecutionID for the Task*/
              processes.getExecution(pdId, ActivityId("BEIS_Wait_Application")).flatMap{
                case Some(executionId) =>{

                  /** Update Activiti Execution to Signal the Waiting Task to release by sending ExecutionID**/
                  val s  = ActionId("signal")
                  val pv =  ProcessVariable("approvestatus", "Submitted")

                  if(bpmreqd.equals("true")) { ///Is there any need of Back office processing?
                    processes.sendSignal(executionId, Action(s, Seq(pv))).flatMap {
                      case Some(executionId) => {
                        /** Update Appplication record with Submit status **/
                        applications.submit(id)
                      }
                      case _ => Future.successful(None)
                    }
                  }else
                    applications.submit(id)

                }
                case _ => Future.successful(None)
              }
            }
          }
      case false =>
          applications.submit(id)
    }
  }
//-------------Simple App Starts--------------------------------------------------------------

  def doSaveSimple(app: ApplicationSectionDetail, fieldValues: JsObject): Future[Result] = {
    app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm | DynamicTableForm => {
        if (JsonHelpers.allFieldsEmpty(fieldValues)) applications.deleteSection(app.id, app.sectionNumber)
        else applications.saveSection(app.id, app.sectionNumber, fieldValues)
      }.map(_ => redirectToSimpleFormOverview(app.id))
      case SectionTypeCostList => Future.successful(redirectToSimpleFormOverview(app.id))
      case SectionTypeFileList => Future.successful(redirectToSimpleFormOverview(app.id))
    }
  }

  def doCompleteSimple(app: ApplicationSectionDetail, fieldValues: JsObject, userId: String): Future[Result] = {
    val answers = app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm | DynamicTableForm => fieldValues
      // Instead of using the values that were passed in from the form we'll use the values that
      // have already been saved against the item list, since these were created by the add-item
      // form.
      case SectionTypeCostList => app.section.map(_.answers).getOrElse(JsObject(Seq()))
      case SectionTypeFileList => app.section.map(_.answers).getOrElse(JsObject(Seq()))
    }

    applications.completeSection(app.id, app.sectionNumber, answers).map {
      case Nil => redirectToSimpleFormOverview(app.id)
      case errs => redisplaySimpleSectionForm(app, answers, errs, userId)
    }
  }

  def doSaveItemSimple(app: ApplicationSectionDetail, fieldValues: JsObject, userId: String): Future[Result] = {
    JsonHelpers.allFieldsEmpty(fieldValues) match {
      case true => applications.deleteSection(app.id, app.sectionNumber).map(_ => redirectToSimpleFormOverview(app.id))
      case false => applications.saveItem(app.id, app.sectionNumber, fieldValues).flatMap {
        case Nil => Future.successful(redirectToSimpleFormOverview(app.id))
        case errs => Future.successful(redisplaySimpleSectionForm(app, fieldValues, errs, userId))
      }
    }
  }

  def doSaveFileItemSimple(app: ApplicationSectionDetail, fieldValues: JsObject): Future[Result] = {
    JsonHelpers.allFieldsEmpty(fieldValues) match {
      case true => applications.deleteSection(app.id, app.sectionNumber).map(_ => redirectToSimpleFormOverview(app.id))
      case false => applications.saveFileItem(app.id, app.sectionNumber, fieldValues).flatMap {
        case itemnumber => Future.successful(redirectToSimpleFormOverview(app.id))
        //case errs => Future.successful(redisplaySectionForm(app, fieldValues, errs))
      }
    }
  }

  def doPreviewSimple(app: ApplicationSectionDetail, fieldValues: JsObject, userId: String): Future[Result] = {
    app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm | DynamicTableForm =>
        val errs = check(fieldValues, previewChecksFor(app.formSection))
        if (errs.isEmpty) applications.saveSection(app.id, app.sectionNumber, fieldValues).map(_ => redirectToPreview(app.id, app.sectionNumber))
        else Future.successful(redisplaySimpleSectionForm(app, fieldValues, errs, userId))

      case SectionTypeCostList => Future.successful(redirectToPreview(app.id, app.sectionNumber))
      case _ => Future.successful(NotFound)
    }
  }

  def doSubmitSimple(id: ApplicationId, applicationDetail: ApplicationDetail, userId: UserId): Future[Option[SubmittedApplicationRef]] = {
          applications.submitSimpleForm(id)
  }

  def redisplaySimpleSectionForm(app: ApplicationSectionDetail, answers: JsObject, errs: FieldErrors = noErrors, userId: String): Result = {
    selectSimpleSectionForm(app, answers, errs, userId)
  }

  def selectSimpleSectionForm(app: ApplicationSectionDetail, answers: JsObject, errs: FieldErrors, userId: String): Result = {
    val checks = app.formSection.fields.map(f => f.name -> f.check).toMap
    val hints = hinting(answers, checks)

    app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm | DynamicTableForm => Ok(views.html.sectionSimpleForm(app, answers, errs, hints))
      /*case SectionTypeCostList =>
        answers \ "items" match {
          case JsDefined(JsArray(is)) if is.nonEmpty =>
            val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
            val costItems = itemValues.flatMap(_.validate[CostItem].asOpt)
            Ok(views.html.sectionList(app, costItems, answers, errs, hints))
          case _ => Redirect(controllers.routes.CostController.addItem(app.id, app.formSection.sectionNumber))
        }*/
      case SectionTypeFileList => {
        val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
        val fileUploadItems = itemValues.flatMap(_.validate[FileUploadItem].asOpt)
        Ok(views.html.sectionSimpleFileList(app, fileUploadItems, answers, errs, hints))
      }

    }
  }
  //----------Simple App ends-----------------------------------------------------------------

  def processVariables(applicationDetail: ApplicationDetail, userId: UserId): Seq[ProcessVariable] ={
    val pvAppId       =  ProcessVariable("ApplicationId", applicationDetail.id.id.toString)
    val pvApplicant   =  ProcessVariable("Applicant", userId.userId)
    val status        =  ProcessVariable("approvestatus", "Submitted")
    val pvAppRef      =  ProcessVariable("ApplicationReference", applicationDetail.personalReference.getOrElse("Not set").toString)
    val pvOpId        =  ProcessVariable("OpportunityId", applicationDetail.opportunity.id.id.toString())
    val pvOpTitle     =  ProcessVariable("OpportunityTitle", applicationDetail.opportunity.title)
    Seq(pvAppId, pvApplicant, status, pvAppRef, pvOpId, pvOpTitle)
  }

  def completeAndPreview(app: ApplicationSectionDetail, fieldValues: JsObject, userId: String): Future[Result] = {
    val answers = app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm | DynamicTableForm => fieldValues
      // Instead of using the values that were passed in from the form we'll use the values that
      // have already been saved against the item list, since these were created by the add-item
      // form.
      case SectionTypeCostList => app.section.map(_.answers).getOrElse(JsObject(Seq()))
      case _ => JsObject(Seq())
    }

    val previewCheckErrs = check(answers, previewChecksFor(app.formSection))
    if (previewCheckErrs.isEmpty) {
      JsonHelpers.allFieldsEmpty(answers) match {
        case true => applications.deleteSection(app.id, app.sectionNumber).map(_ => redirectToOverview(app.id))
        case false => applications.completeSection(app.id, app.sectionNumber, answers).map {
          case Nil => redirectToPreview(app.id, app.sectionNumber)
          case errs => redisplaySectionForm(app, answers, errs, userId)
        }
      }
    } else Future.successful(redisplaySectionForm(app, answers, previewCheckErrs, userId))
  }

  def redirectToPreview(id: ApplicationId, sectionNumber: AppSectionNumber) =
    Redirect(routes.ApplicationPreviewController.previewSection(id, sectionNumber))

  def renderSectionForm(app: ApplicationSectionDetail,
                        errs: FieldErrors,
                        hints: FieldHints,
                        userId: String): Result = {
    val answers = app.section.map { s => s.answers }.getOrElse(JsObject(List.empty))
    selectSectionForm(app, answers, errs, userId)
  }

  def redirectToSimplePreview(id: ApplicationId, sectionNumber: AppSectionNumber) =
    Redirect(routes.ApplicationPreviewController.previewSection(id, sectionNumber))

  def renderSectionSimpleForm(app: ApplicationSectionDetail,
                        errs: FieldErrors,
                        hints: FieldHints): Result = {
    val answers = app.section.map { s => s.answers }.getOrElse(JsObject(List.empty))
    selectSectionSimpleForm(app, answers, errs)
  }

  def redisplaySectionForm(app: ApplicationSectionDetail, answers: JsObject, errs: FieldErrors = noErrors, userId: String): Result = {
    selectSectionForm(app, answers, errs, userId)
  }

  def selectSectionForm(app: ApplicationSectionDetail, answers: JsObject, errs: FieldErrors, userId: String): Result = {
    val checks = app.formSection.fields.map(f => f.name -> f.check).toMap
    val hints = hinting(answers, checks)

    app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm =>
        Ok(views.html.sectionForm(app, answers, errs, hints, userId, Option(guidanceDocURL)))

      case SectionTypeCostList =>
        answers \ "items" match {
          case JsDefined(JsArray(is)) if is.nonEmpty =>
            val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
            val costItems = itemValues.flatMap(_.validate[CostItem].asOpt)
            Ok(views.html.sectionList(app, costItems, answers, errs, hints, userId))
          case _ => Redirect(controllers.routes.CostController.addItem(app.id, app.formSection.sectionNumber))
        }
      case SectionTypeFileList => {
        val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
        val fileUploadItems = itemValues.flatMap(_.validate[FileUploadItem].asOpt)

        Ok(views.html.sectionFileList(app, fileUploadItems, answers, errs, hints, userId, publicDownloadURL, Option(guidanceDocURL)))
      }
      case DynamicTableForm => {
        val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
        val tableItems = itemValues.flatMap(_.validate[DynamicTableItem].asOpt)

        val dform = app.formSection.fields.map{f=>
            f.asInstanceOf[forms.DynamicTableFormField]
        }
        Ok(views.html.sectionDynamicTDList(app, dform.head, tableItems, answers, errs, hints, userId))
      }
    }
  }

  def guidanceDocURL = {
    val guidancedoc = Config.config.file.guidancedoc
    s"$publicDownloadURL/$guidancedoc"
  }


  val publicDownloadURL  ={
    val amazonRegion = Config.config.aws.region
    val amazonDomain = Config.config.aws.domain
    val amazonPublicBucket = Config.config.aws.publicbucket
    val amazonPublicDownloadUrl = s"https://s3.$amazonRegion.$amazonDomain/$amazonPublicBucket"
    amazonPublicDownloadUrl
  }

  def selectSectionSimpleForm(app: ApplicationSectionDetail, answers: JsObject, errs: FieldErrors): Result = {
    val checks = app.formSection.fields.map(f => f.name -> f.check).toMap
    val hints = hinting(answers, checks)

    app.formSection.sectionType match {
      case SectionTypeForm | SimpleTypeForm | RowForm | TableForm => Ok(views.html.sectionSimpleForm(app, answers, errs, hints))
      /*case SectionTypeCostList =>
        answers \ "items" match {
          case JsDefined(JsArray(is)) if is.nonEmpty =>
            val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
            val costItems = itemValues.flatMap(_.validate[CostItem].asOpt)
            Ok(views.html.sectionList(app, costItems, answers, errs, hints))
          case _ => Redirect(controllers.routes.CostController.addItem(app.id, app.formSection.sectionNumber))
        }*/
      case SectionTypeFileList => {
        val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
        val fileUploadItems = itemValues.flatMap(_.validate[FileUploadItem].asOpt)
        Ok(views.html.sectionSimpleFileList(app, fileUploadItems, answers, errs, hints))
      }

    }
  }

  def previewChecksFor(formSection: ApplicationFormSection): Map[String, FieldCheck] =
    //TODO:- may need to implement seperate checks for Previews
    //formSection.fields.map(f => f.name -> f.previewCheck).toMap
    formSection.fields.map(f => f.name -> f.check).toMap  //may need to implement seperate checks for Previews


  def isAuthTokenValid(token: String, id:Option[ApplicationId] = None) = {

    val appAccessRole = Config.config.jwt.appAccessRole

    //This isValidToken method just validates the token with SecretKey.
    // Dont validate payload (Exp time, Appid, orRole)
    if(token != null && jwt.isValidToken(token) ){

      val payload = jwt.decodePayload(token)

      val authAttribs =
        ((Json.parse(payload.getOrElse("")) \ "role").validate[String].getOrElse(""),
          (Json.parse(payload.getOrElse("")) \ "appid").validate[String].getOrElse(""),
          (Json.parse(payload.getOrElse("")) \ "exp").validate[Long].getOrElse(0L))

      val jwtRole  = authAttribs._1
      val jwtAppId = authAttribs._2
      val exp      = authAttribs._3

      val aid = id.isEmpty match {
        case false => id.get.id.toString()
        case true => ""
      }

      /** 1. The JWT token created by Process management server to access Front-end resource, it will have
        *    role, Appid, and Exp attributes in payload
        * 2. The JWT token created by Front-end server to access its own resource (eg. download link etc),
        *    Exp (Expiration time) only sent in the token and is validated on exp value only.
      **/

      /*jwtRole.equals(appAccessRole) &&*/ aid.equals(jwtAppId) && !isTokenExpired(exp)
    }
    else
      false
  }

  def isTokenExpired(datetime : Long) = {
    new DateTime(datetime).isBeforeNow

  }

  def getValueFromRequest(key: String, keyValueMap: Map[String, Seq[String]]): String =

    keyValueMap.get(key).headOption.map(_.head).getOrElse("").toString

}
