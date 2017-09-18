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
import play.api.libs.json._

case class DynamicTableFormField(label: Option[String], name: String, dynamictableform : Seq[TableField]) extends Field {
  implicit val dynamicTableFormReads = Json.reads[SimpleFormValues]
  implicit val tableRowDataReads = Json.reads[TableRowData]
  implicit val dynamictableItemReads = Json.reads[DynamicTableItem]

  //override def check: FieldCheck = FieldChecks.fromValidator(ContactValidator)
  //override def check: FieldCheck = FieldChecks.fromValidator(new ContactValidator(Seq(telephoneField, emailField, webField, twitterField)))
  //override def check: FieldCheck = FieldChecks.noCheck


  override def check: FieldCheck = FieldChecks.noCheck

  override def previewCheck: FieldCheck = FieldChecks.mandatoryCheck


  override def renderFormInput(questions: Map[String, Question], answers: JsObject, errs: Seq[FieldError], hints: Seq[FieldHint]) = {

    val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
    val tableItems = itemValues.flatMap(_.validate[DynamicTableItem].asOpt)
    views.html.renderers.dynamicTableformField(this, questions, tableItems, errs, hints)
  }

  override def renderPreview(questions: Map[String, Question], answers: JsObject) = {
    val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
    val tableItems = itemValues.flatMap(_.validate[DynamicTableItem].asOpt)
    views.html.renderers.preview.dynamicTableFormField(this, tableItems)
  }

}
case class DynamicTableItem(tableRowData: Seq[String], itemNumber: Option[Int] = None)
