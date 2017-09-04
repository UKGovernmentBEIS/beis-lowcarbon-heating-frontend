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

package forms.validation

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.cartesian._
import cats.syntax.traverse._
import cats.instances.list._
import config.Config
import forms.TextField
import forms.validation.FieldValidator.Normalised
import play.api.libs.json.JsObject

case class ContactValues(telephone: Option[String], email: Option[String], web: Option[String] = None, twitter: Option[String] = None)

case class Contact(telephone: String, email: String, web: String, twitter: String)

 object ContactValidator  {
  def apply(textfields : Seq[TextField]) =
    new ContactValidator(textfields)
}

class ContactValidator(textfields : Seq[TextField]) extends FieldValidator[JsObject, List[(TextField, String)]]{

  override def doValidation(path: String, contactValues: Normalised[JsObject]): ValidatedNel[FieldError, List[(TextField, String)]] = {

    def createValidator(f : String) = {
      val textfield = textfields.filter(t => t.name == s"$path.$f")
      textfield.head.isMandatory match {
        case true =>
          MandatoryValidator(Some(f)).andThen(CharacterCountValidator(None, textfield.head.maxWords))
        case false => NonMandatoryValidator(None)
      }
    }

    /*val telephoneValidator = createValidator("telephone").validate(s"$path.telephone", contactValues.telephone)
    val emailValidator = createValidator("email").validate(s"$path.email", contactValues.email)
    val webValidator = createValidator("web").validate(s"$path.web", contactValues.web)
    val twitterValidator = createValidator("twitter").validate(s"$path.twitter", contactValues.twitter)*/


    val fldCheck: ValidatedNel[FieldError, List[(TextField, String)]] = textfields.toList.traverseU { a =>
      val fld = a.name
      val contactOptJsValue = contactValues.value.get(a.name.split("\\.").last)
      val contactOptString = contactOptJsValue.flatMap {j=> j.asOpt[String]}
      createValidator(a.name.split("\\.").last).validate(s"$fld", contactOptString ).map(v => (a,v))
    }

    fldCheck
  }

}
