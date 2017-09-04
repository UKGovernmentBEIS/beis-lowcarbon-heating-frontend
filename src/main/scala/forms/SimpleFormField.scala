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
import forms.validation.{ContactValidator, _}
import models._
import play.api.libs.json._

case class SimpleFormField(name: String, simpleform : Seq[SimpleField]) extends Field {
  implicit val simpleFormReads = Json.reads[SimpleFormValues]

  override def check: FieldCheck = {
    simpleform.map{ d=>
      println("dddd============== "+d.label + "--" + d.isMandatory)
    }
    //FieldChecks.fromValidator(new SimpleFormValidator(Seq(telephoneField, emailField, webField, twitterField)))
    FieldChecks.fromValidator(new SimpleFormValidator(simpleform))
  }

  override def previewCheck: FieldCheck = FieldChecks.mandatoryCheck

  override def renderPreview(questions: Map[String, Question], answers: JsObject) = {
    views.html.renderers.preview.simpleFormField(this, JsonHelpers.flatten(answers))
  }

  override def renderFormInput(questions: Map[String, Question], answers: JsObject, errs: Seq[FieldError], hints: Seq[FieldHint]) = {


//    val itemValues: Seq[JsValue] = (answers \ "items").validate[JsArray].asOpt.map(_.value).getOrElse(Seq())
//    val fileUploadItems = itemValues.flatMap(_.validate[FileUploadItem].asOpt)
//    views.html.renderers.preview.fileUpload(fileUploadItems)


    views.html.renderers.simpleFormField(this, questions, JsonHelpers.flatten(answers), errs, hints)
  }
}
