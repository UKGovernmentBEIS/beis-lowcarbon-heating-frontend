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

package forms

import controllers.{FieldCheck, FieldChecks, JsonHelpers}
import forms.validation._
import models._
import play.api.Logger
import play.api.libs.json._
import services.RestService.{JsonParseException, RestFailure}

case class TableFormField(name: String, tableform : Seq[TableField]) extends Field {
  implicit val tableFormReads = Json.reads[SimpleFormValues]
  implicit val tableRowDataReads = Json.reads[TableRowData]
  implicit val tableFormDataReads = Json.reads[TableFormData]
  //implicit val tableObjectReads = Json.format[Option[play.api.libs.json.JsObject]]

  override def check: FieldCheck = {

      /*val rows = tableform.flatMap(_.fields)
      FieldChecks.fromValidator(new TableFormValidator(rows))*/
      //FieldChecks.fromValidator(new TableFormValidator(tableform)
      FieldChecks.noCheck
  }

  override def previewCheck: FieldCheck = FieldChecks.mandatoryCheck

  override def renderPreview(questions: Map[String, Question], answers: JsObject) = {
    views.html.renderers.preview.tableFormField(this, JsonHelpers.flatten(answers))
  }

  override def renderFormInput(questions: Map[String, Question], answers: JsObject, errs: Seq[FieldError], hints: Seq[FieldHint]) = {
    answers.validate[TableFormData] match {
      case JsSuccess(dynamicTableFormFieldAnsers, _) =>
        Logger.debug("Validated in renderFormInput:-" + dynamicTableFormFieldAnsers.rowsData.toString())
        // views.html.renderers.tableFormField(this, questions, dynamicTableFormFieldAnsers, errs, hints)
      case JsError(errs) =>
        Logger.debug("Errors in renderFormInput:-" + errs)
    }

    views.html.renderers.tableFormField(this, questions, JsonHelpers.flatten(answers), errs, hints)
  }
}
