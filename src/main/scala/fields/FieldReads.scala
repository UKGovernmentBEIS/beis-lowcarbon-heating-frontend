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

package fields

import forms._
import play.api.libs.json._

case class FieldType(`type`: String)

object FieldReads {

  implicit val fieldTypeReads = Json.reads[FieldType]
  implicit val textFieldReads = Json.reads[TextField]
  implicit val textAreaFieldReads = Json.reads[TextAreaField]
  implicit val dateWithDaysReads = Json.reads[DateWithDaysField]
  implicit val dateTimeRangeReads = Json.reads[DateTimeRangeField]
  implicit val costItemReads = Json.reads[CostItemField]
  implicit val fileUploadReads = Json.reads[FileUploadField]
  implicit val addressFieldReads = Json.reads[AddressField]
  implicit val contactFieldReads = Json.reads[ContactField]
  implicit val companyInfoFieldReads = Json.reads[CompanyInfoField]
  implicit val rowFieldReads = Json.reads[RowField]
  implicit val rowFormFieldReads = Json.reads[RowFormField]
  implicit val tableRowReads = Json.reads[TableRow]
  implicit val tableFieldReads = Json.reads[TableField]
  implicit val fileUploadItemReads = Json.reads[FileUploadItem]
  implicit val simpleFieldReads = Json.reads[SimpleField]
  implicit val simpleFormFieldReads = Json.reads[SimpleFormField]
  implicit val tableFormFieldReads = Json.reads[TableFormField]
  implicit val dynamictableFormFieldReads = Json.reads[DynamicTableFormField]

  implicit object fieldReads extends Reads[Field] {
    override def reads(json: JsValue): JsResult[Field] = {
      //System.out.println("FieldType--------------------------" + json)
      json.validate[FieldType].flatMap { o =>
        o.`type` match {
          case "text" => json.validate[TextField]
          case "textArea" => json.validate[TextAreaField]
          case "dateWithDays" => json.validate[DateWithDaysField]
          case "dateTimeRange" => json.validate[DateTimeRangeField]
          case "costItem" => json.validate[CostItemField]
          case "fileUpload" => json.validate[FileUploadField]
          case "address" => json.validate[AddressField]
          case "contact" => json.validate[ContactField]
          case "companyInfo" => json.validate[CompanyInfoField]
          case "simpleform" => json.validate[SimpleFormField]
          case "rowform" => json.validate[RowFormField]
          case "tableform" => json.validate[TableFormField]
          case "dynamictableform" => json.validate[DynamicTableFormField]
          case t => JsError(s"unknown field type $t")
        }
      }
    }
  }

}
