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
import controllers.FieldChecks
import forms.{SimpleField, TextField}
import forms.validation.FieldValidator.Normalised
import play.api.libs.json.{JsObject, JsString, Json}
import forms.DateValues
case class SimpleFormValues(employeename: Option[String]=None, department: Option[String], natureofillness: Option[String],
                             managername: Option[String], manageremail: Option[String])
case class SimpleForm(employeename: String, department: String, natureofillness: String,
                            managername: String, manageremail: String)


object SimpleFormValidator  {
  def apply(textfields : Seq[SimpleField]) = new SimpleFormValidator(textfields)
}

class SimpleFormValidator(textfields : Seq[SimpleField]) extends FieldValidator[JsObject, List[(SimpleField, String)]]{

  implicit val dvReads = Json.reads[DateValues]

  def validator(l:Option[String]= None, n:String, m:Int) = MandatoryValidator(l, Option(n)).andThen(CharacterCountValidator(l, m))
  def validatorTextArea(l:Option[String]= None, n:String, m:Int) = MandatoryValidator(l, Option(n)).andThen(WordCountValidator(l, m))
  def validatorDate(b:Boolean) = DateFieldValidator(None, b)
  def validatorCurrency(l:Option[String]= None): CurrencyValidator = CurrencyValidator(l)//CurrencyValidator.anyValue
  def validatorTextInt(l:Option[String]= None, n:String) = IntValidator(l)
  def validatorCheckbox(l:Option[String]= None, n:String) = MandatoryValidator(l,Option(n))

  override def doValidation(path: String, fldValues: Normalised[JsObject]): ValidatedNel[FieldError, List[(SimpleField, String)]] = {

    def createValidator(f : String) = {

      val textfield = textfields.filter(t => t.name == s"$path.$f")
      val fld = textfield.head

      fld.isMandatory.getOrElse(false) match {
        case true => {
          fld.fieldType match {
            case "textarea" =>
              validatorTextArea(fld.label, fld.name, fld.maxWords)
//            case "currency" =>
//              validatorTextArea(fld.label, fld.name, fld.maxWords)
            case "checkbox" =>
              validatorCheckbox(fld.label, fld.name)
            case _ =>
              validator(fld.label, fld.name, fld.maxWords)
          }
        }
        case false => NonMandatoryValidator(None)
      }
    }

    val validChecks: ValidatedNel[FieldError, List[(SimpleField, String)]] = textfields.toList.traverseU { a =>
      val nameWithPath = a.name
      val nameWithoutPath = a.name.split("\\.").last
      val fldOptJsValue = fldValues.value.get(nameWithoutPath)

      System.out.println("====fldOptJsValue  fldOptJsValue fldOptJsValue===="+ fldValues + "==="+ nameWithPath + "==="+ fldOptJsValue)


      a.fieldType match {
        case "String" => NonMandatoryValidator(None).validate("", Option("")).map(v => (a, ""))
        case "text" =>
          a.isNumeric.getOrElse(false) match {
            case true =>
              val fldOptString = fldOptJsValue.flatMap {j=> j.asOpt[String]}
              IntValidator(a.label).validate(s"$nameWithPath", fldOptString.getOrElse("0")).map(v => (a, ""))
            case false =>
              val fldOptString = fldOptJsValue.flatMap {j=> j.asOpt[String]}
              createValidator(nameWithoutPath).validate(s"$nameWithPath", fldOptString ).map(v => (a,v))
        }
        case "date" => {
          val datevalues = fldOptJsValue.flatMap { j => j.asOpt[DateValues] }.getOrElse(DateValues(None, None, None))
          DateFieldValidator(a.label, true).validate(s"$nameWithPath", datevalues).map(v => (a, ""))
        }
        case "currency" => {
          val fldOptString = fldOptJsValue.flatMap {j=> j.asOpt[String]}
          validatorCurrency(a.label).validate(s"$nameWithPath", fldOptString).map(v => (a, ""))
        }
        case "checkbox" => {
          val fldOptString = fldOptJsValue.flatMap {j=> j.asOpt[String]}
          createValidator(nameWithoutPath).validate(s"$nameWithPath", fldOptString).map(v => (a, ""))
        }
        case "tableform" => {
          System.out.println("=====k k k k k k ====="+ nameWithoutPath)
          fldValues.value.get(nameWithoutPath) match{
            case Some(v) =>
              System.out.println("=====v v v v v  v ====="+ v)

              val allFlds = a.tableform.getOrElse(Seq()).map {row=>row.fields}.flatten
              val fldJsObject = v.as[JsObject]
              TableFormValidator(allFlds).validate(s"$nameWithPath", fldJsObject).map(v => (a, ""))
            case None => NonMandatoryValidator(None).validate("", Option("")).map(v => (a, ""))
          }

      /*    val fldOptJsValue_ = fldValues.value.getOrElse(nameWithoutPath, Json.parse(""))
          val fldJsObject = fldOptJsValue_.as[JsObject]
//          a.tableform.getOrElse(Seq()).toList.traverseU { row=>
//            TableFormValidator(row.fields).validate(s"$nameWithPath", fldJsObject).map(v => (a, ""))
//          }
          val allFlds = a.tableform.getOrElse(Seq()).map {row=>row.fields}.flatten
          TableFormValidator(allFlds).validate(s"$nameWithPath", fldJsObject).map(v => (a, ""))
          */
        }
        case _ =>{
          val fldOptString = fldOptJsValue.flatMap {j=> j.asOpt[String]}
          createValidator(nameWithoutPath).validate(s"$nameWithPath", fldOptString ).map(v => (a,v))
        }
      }
    }

    validChecks
  }

}
