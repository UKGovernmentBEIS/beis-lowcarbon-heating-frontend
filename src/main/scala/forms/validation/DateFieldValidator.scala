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

import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.cartesian._
import cats.syntax.validated._
import forms.DateValues
import forms.validation.FieldValidator.Normalised
import org.joda.time.LocalDate

import scala.util.Try

case class DMY(day: Int, month: Int, year: Int)
//
//object DateFieldValidator {
//  val mustProvideAValidDateMsg = "Must provide a valid date"
//  val mustBeTodayOrLaterMsg = "Must be today or later"
//}

case class DateFieldValidator(label: Option[String] = None, allowPast: Boolean,
                              minYearValue: Int = Int.MinValue, maxYearValue: Int = Int.MaxValue) extends FieldValidator[DateValues, LocalDate] {

    val mustProvideAValidDateMsg = "Must provide a valid date"
    val mustBeTodayOrLaterMsg = "Must be today or later"

  import DateFieldValidator._

  override def normalise(vs: DateValues): DateValues =
    vs.copy(day = vs.day.map(_.trim()), month = vs.month.map(_.trim()), year = vs.year.map(_.trim()))

  def mandatoryInt(path: String, s: Option[String], displayName: String): ValidatedNel[FieldError, Int] =
    MandatoryValidator(Some(displayName)).validate(path, s).andThen(IntValidator().validate(path, _))

  def validateDMY(path: String, vs: Normalised[DateValues]): ValidatedNel[FieldError, DMY] = {

    def normaliseYear(y: Int) = if (y < 100) y + LocalDate.now.getCenturyOfEra * 100 else y

    (mandatoryInt(s"$path.day", vs.day, "day") |@|
      mandatoryInt(s"$path.month", vs.month, "month") |@|
      mandatoryInt(s"$path.year", vs.year, "year")).tupled
      .map { case (d, m, y) =>
        DMY(d, m, normaliseYear(y))
      }.leftMap(_ => NonEmptyList.of(FieldError(s"$path", s"'${label.getOrElse("Field")}' $mustProvideAValidDateMsg")))
  }

  def validateDate(path: String, dmy: DMY): ValidatedNel[FieldError, LocalDate] =
    Try(new LocalDate(dmy.year, dmy.month, dmy.day)).toOption match {
      case Some(ld) => {
        if(dmy.year < minYearValue)  FieldError(path, s"'${label.getOrElse("Field")}' year value should be greater than $minYearValue").invalidNel
        else if(dmy.year > maxYearValue)  FieldError(path, s"'${label.getOrElse("Field")}' year value should be less than $maxYearValue").invalidNel
        else ld.valid
      }
      case None => FieldError(path, s"'${label.getOrElse("Field")}' $mustProvideAValidDateMsg").invalidNel
    }

  def validatePastDate(path: String, ld: LocalDate): ValidatedNel[FieldError, LocalDate] =
    if (!allowPast && ld.isBefore(LocalDate.now())) FieldError(path, s"'${label.getOrElse("Field")}' mustBeTodayOrLaterMsg").invalidNel
    else ld.valid

  override def doValidation(path: String, vs: Normalised[DateValues]): ValidatedNel[FieldError, LocalDate] =
     validateDMY(path, vs).andThen(validateDate(path, _)).andThen(validatePastDate(path, _))

}
