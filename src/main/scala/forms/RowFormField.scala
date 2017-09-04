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

case class RowField(label: Option[String], name: String, size: Option[Int] = None, isEnabled: Boolean, fieldType: String)

case class RowFormField(name: String, fieldType: String, rowform : Seq[RowField]) extends Field {
  implicit val tableFormReads = Json.reads[SimpleFormValues]
  implicit val rowFieldReads = Json.format[RowField]




  override def check: FieldCheck = {
    //println("== ContactField case true "+name + " ======= " +  contactitems)

    //FieldChecks.fromValidator(new SimpleFormValidator(Seq(telephoneField, emailField, webField, twitterField)))

    //FieldChecks.fromValidator(SimpleFormValidator)
    ???
  }

  override def previewCheck: FieldCheck = FieldChecks.mandatoryCheck

  override def renderPreview(questions: Map[String, Question], answers: JsObject) = {
    views.html.renderers.preview.rowFormField(this, JsonHelpers.flatten(answers))
  }

  override def renderFormInput(questions: Map[String, Question], answers: JsObject, errs: Seq[FieldError], hints: Seq[FieldHint]) = {

    views.html.renderers.rowFormField(this, questions, JsonHelpers.flatten(answers), errs, hints)
  }


}