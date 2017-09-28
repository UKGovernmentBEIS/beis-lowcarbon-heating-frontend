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

import cats.data.ValidatedNel
import cats.syntax.validated._
import forms.validation.FieldValidator.Normalised

import scala.util.Try

object CurrencyValidator {
  def apply() = new CurrencyValidator(None, None, None)

  def apply(minValue: BigDecimal) = new CurrencyValidator(None, Some(minValue), None)

  def apply(label: String) = new CurrencyValidator(Some(label), None, None)

  def apply(label: String, minValue: BigDecimal, maxValue: BigDecimal) = new CurrencyValidator(Some(label), Some(minValue), Some(maxValue))

  final val greaterThanZero = apply(BigDecimal(0.0))
  final val anyValue = apply()
}

class CurrencyValidator(label:  Option[String], minValue: Option[BigDecimal], maxValue: Option[BigDecimal]) extends FieldValidator[Option[String], BigDecimal] {
  override def normalise(os: Option[String]): Option[String] = os.map(_.trim().replaceAll(",", ""))

  override def doValidation(path: String, value: Normalised[Option[String]]): ValidatedNel[FieldError, BigDecimal] = {

    Try(BigDecimal(value.getOrElse("")).setScale(2, BigDecimal.RoundingMode.HALF_UP)).toOption match {
      case Some(a) =>  {

        if(minValue.nonEmpty && maxValue.nonEmpty){
          if(a < minValue.get)
            FieldError(path, s"'${label.getOrElse("Field")}' The value must be greater than ${minValue.get}").invalidNel
          else if(a > maxValue.get)
            FieldError(path, s"'${label.getOrElse("Field")}' The value must be less than ${maxValue.get}").invalidNel
          else
            a.validNel
       }
        else
        a.validNel
      }
      case None => FieldError(path, s"'${label.getOrElse("Field")}' Must be a valid currency value").invalidNel
    }
  }
}