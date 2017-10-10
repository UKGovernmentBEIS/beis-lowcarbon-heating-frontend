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
import forms.{TableRow, _}
import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.{JsObject, JsString, _}

class TableFormValidatorSpec extends WordSpecLike with Matchers {
  "Table form fields validator" should {

  /*SimpleField(label: Option[String], name: String, helptext: Option[String]= None, isEnabled: Boolean, isMandatory: Option[Boolean]=None,
  isNumeric: Option[Boolean]= None, size : Option[Int] = None,  maxWords: Int, fieldType: String,
  rowform : Option[Seq[RowField]] = None, tableform : Option[Seq[TableField]] = None,
  filelist: Option[Seq[FileUploadItem]] = None, defaultvalue: Option[String],
  minYearValue: Option[Int]= None, maxYearValue: Option[Int]= None,
  minValue: Option[Int]= None, maxValue: Option[Int]= None) extends Field {

  TableField(label: Option[String], name: String, helptext: Option[String]= None, isEnabled: Option[Boolean]=None,
             isMandatory: Option[Boolean]= None, isNumeric: Option[Boolean]= None, size : Option[Int] = None,
             fieldType: Option[String]=None, maxWords: Option[Int]=None, fields: Seq[TableRow])  extends Field {

  TableRow(label: Option[String], name: String,  isNumeric: Option[Boolean] = None,  maxWords: Option[Int],
                    isEnabled: Boolean, isMandatory: Option[Boolean]= None, fieldType: String,
                    minYearValue: Option[Int]= None, maxYearValue: Option[Int]= None,
                    minValue: Option[Int]= None, maxValue: Option[Int]= None)
  */

    val tableField = TableField(None,"costmetrics.table.row5",None,Some(true),None,None,None,None,None,
      List(
        TableRow(Some("Annual Maintenance costs"),"costmetrics.table.row5col1.text",None,None,true,Some(true),"text",None,None,None,None),
        TableRow(None,"costmetrics.table.row5col2.currency",None,None,true,Some(false),"currency",None,None,None,None),
        TableRow(None,"costmetrics.table.row5col3.int",None,None,true,Some(false),"int",None,None,None,None)
      )
    )

    val tableRowWithTextField = TableRow(Some("Annual Maintenance costs text"),"costmetrics.table.row5col1.text",None,None,true,Some(true),"text",None,None,None,None)
    val tableRowWithCurrencyFiled = TableRow(Some("Annual Maintenance costs currency"),"costmetrics.table.row5col2.currency",None,None,true,Some(true),"currency",None,None,None,None)
    val tableRowWithIntFiled = TableRow(Some("Annual Maintenance costs int"),"costmetrics.table.row5col3.int",Some(true),None,true,Some(true),"text",None,None,Some(10),Some(100))

      //=Some({"row6col3":"EE","row4col3":"","row2col2":"FF","row5col2":"EE","row2col3":"gg","row4col2":"","row3col2":"111","row3col3":"222","row6col2":"CBC","row5col3":""})

    val jsobjTableRow:JsObject = JsObject(Seq(
      "text" -> JsString("abcdefghijklmn"),
      "currency" -> JsString("2000abc£"),
      "int" -> JsString("333XX")
    ))

    val jsobjTableRowCurrencyOK:JsObject = JsObject(Seq(
      "currency" -> JsString("2222")
    ))

    val jsobjTableRowIntNOTOK1:JsObject = JsObject(Seq(
      "int" -> JsString("200")
    ))

    val jsobjTableRowIntNOTOK2:JsObject = JsObject(Seq(
      "int" -> JsString("5")
    ))


    "reject a missing value of text field when it is mandatory" in {

      TableFormValidator(Seq(tableRowWithTextField)).validate("costmetrics.table.row5col1.text", jsobjTableRow - "text")
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Annual Maintenance costs text' cannot be empty"
      } shouldBe  an[Invalid[_]]
    }

    "reject a missing value of currency field when it is mandatory" in {

      TableFormValidator(Seq(tableRowWithCurrencyFiled)).validate("costmetrics.table.row5col2.currency", jsobjTableRow - "currency")
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Annual Maintenance costs currency' cannot be empty"
      } shouldBe  an[Invalid[_]]
    }

    "reject a missing value of int field when it is mandatory" in {

      TableFormValidator(Seq(tableRowWithIntFiled)).validate("costmetrics.table.row5col2.currency", jsobjTableRow - "int")
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Annual Maintenance costs int' cannot be empty"
      } shouldBe  an[Invalid[_]]
    }


    "Rejects a value of currency field when it is mandatory and it is not a currency value" in {

      TableFormValidator(Seq(tableRowWithCurrencyFiled)).validate("costmetrics.table.row5col2.currency", jsobjTableRow)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Annual Maintenance costs currency' Must be a valid currency value"
      } shouldBe  an[Invalid[_]]
    }

    "acctept value of currency field when it is mandatory and it is currency value" in {

      TableFormValidator(Seq(tableRowWithCurrencyFiled)).validate("costmetrics.table.row5col2.currency", jsobjTableRowCurrencyOK)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Annual Maintenance costs currency' Must be a valid currency value"
      } shouldBe  an[Valid[_]]
    }

    "Rejects a value of int field when it is mandatory and it is not a int value" in {

      TableFormValidator(Seq(tableRowWithIntFiled)).validate("costmetrics.table.row5col3.int", jsobjTableRow)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Annual Maintenance costs int' must be a whole number"
      } shouldBe  an[Invalid[_]]
    }

    "Rejects a value of int field when it is greater than maxValue allowed" in {

      TableFormValidator(Seq(tableRowWithIntFiled)).validate("costmetrics.table.row5col3.int", jsobjTableRowIntNOTOK1)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Annual Maintenance costs int' Maximum value is 100"
      } shouldBe  an[Invalid[_]]
    }

    "Rejects a value of int field when it is less than minValue allowed" in {

      TableFormValidator(Seq(tableRowWithIntFiled)).validate("costmetrics.table.row5col3.int", jsobjTableRowIntNOTOK2)
        .map{ e =>
        }.leftMap{err =>
        err.head.err shouldBe "'Annual Maintenance costs int' Minimum value is 10"
      } shouldBe  an[Invalid[_]]
    }


  }
}
