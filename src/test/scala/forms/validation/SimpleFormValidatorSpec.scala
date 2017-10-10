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

import cats.data.Validated.{Invalid, Valid}
import forms._
import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.Jsonp
import play.api.libs.json.{JsObject, JsString}
import play.api.libs.json._

class SimpleFormValidatorSpec extends WordSpecLike with Matchers {
  "Simple form fields validator" should {

    /*SimpleField(label: Option[String], name: String, helptext: Option[String]= None, isEnabled: Boolean, isMandatory: Option[Boolean]=None,
  isNumeric: Option[Boolean]= None, size : Option[Int] = None,  maxWords: Int, fieldType: String,
  rowform : Option[Seq[RowField]] = None, tableform : Option[Seq[TableField]] = None,
  filelist: Option[Seq[FileUploadItem]] = None, defaultvalue: Option[String],
  minYearValue: Option[Int]= None, maxYearValue: Option[Int]= None,
  minValue: Option[Int]= None, maxValue: Option[Int]= None) extends Field {*/


    val textField = SimpleField(Some("Project name"),"proposalsummary.text",None,true,Some(true),Some(false),None,10,
      "text",None,None,None,None,None,None,None,None)

    val currencyField = SimpleField(Some("Currency field (£)"),"proposalsummary.currency",None,true,Some(true),Some(true),None,10,
      "currency",None,None,None,None,None,None,Some(0),Some(2000000))

    val dateField = SimpleField(Some("Estimated Start Date"),"proposalsummary.date",None,true,Some(true),Some(true),None,100,
             "date",None,None,None,None,None,Some(2050),None,None)

    val intField = SimpleField(Some("Project Duration (months)"),"proposalsummary.int",None,true,Some(true),Some(true),None,1,
      "text",None,None,None,None,None,None,Some(10),Some(36))

    val jsobjNoFields:JsObject = JsObject(Seq())

    val jsobj:JsObject = JsObject(Seq(
      "text" -> JsString("abcdefghijklmn"),
      "currency" -> JsString("2000005"),
      "date" -> //JsString(
        Json.toJson(
          JsObject (Seq("day" -> JsString("11"), "month" -> JsString("11"), "year" -> JsString("2099")))
        ),
      "int" -> JsString("5")
    ))

    val jsobjNonNumeric:JsObject = JsObject(Seq(
      "currency" -> JsString("20000abcd05")
    ))

    "reject a missing value of text field when it is mandatory" in {

      SimpleFormValidator(Seq(textField)).validate("proposalsummary.text", jsobjNoFields)
        .map{ e =>
      }.leftMap{err =>
        err.head.err shouldBe "'Project name' cannot be empty"
      } shouldBe  an[Invalid[_]]
    }

    "reject a missing value of currency field when it is mandatory" in {

      SimpleFormValidator(Seq(currencyField)).validate("proposalsummary.currency", jsobjNoFields)
      .map{ e =>
      }.leftMap{err =>
      err.head.err shouldBe "'Currency field (£)' cannot be empty"
    } shouldBe  an[Invalid[_]]
    }

    "reject a missing value of date field when it is mandatory" in {

        SimpleFormValidator(Seq(dateField)).validate("proposalsummary.date", jsobjNoFields)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Estimated Start Date' cannot be empty"
      }   shouldBe  an[Invalid[_]]
    }

    "reject a value of date field when it's Max Year Value is greater than required year" in {

      SimpleFormValidator(Seq(dateField)).validate("proposalsummary.date", jsobj)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Estimated Start Date' year value should be less than 2050"
      }   shouldBe  an[Invalid[_]]
    }

    "accept text field when the number of max characters is less than mandatory" in {

      SimpleFormValidator(Seq(textField)).validate("proposalsummary", jsobj)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Project name' must be a whole number"
      }   shouldBe  an[Valid[_]]
    }

    "reject currency field when the value is not a numeric" in {

      SimpleFormValidator(Seq(currencyField)).validate("proposalsummary", jsobjNonNumeric)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Currency field (£)' Must be a valid currency value"
      }   shouldBe  an[Invalid[_]]
    }

    "reject currency field when the number of max characters is less than mandatory" in {

      SimpleFormValidator(Seq(currencyField)).validate("proposalsummary", jsobj)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Currency field (£)' The value must be less than 2000000"
      }   shouldBe  an[Invalid[_]]
    }

    "reject int field when the number of min characters is higher than mandatory" in {

      SimpleFormValidator(Seq(intField)).validate("proposalsummary", jsobj)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Project Duration (months)' Minimum value is 10"
      }   shouldBe  an[Invalid[_]]
    }

    //Dynamic Table


   }
}
