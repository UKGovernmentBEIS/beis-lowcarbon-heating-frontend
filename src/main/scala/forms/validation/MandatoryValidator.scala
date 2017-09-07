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

case class MandatoryValidator(label: Option[String] = None, displayName: Option[String] = None) extends FieldValidator[Option[String], String] {
  override def normalise(os: Option[String]): Option[String] = os.map(_.trim())

  override def doValidation(path: String, so: Normalised[Option[String]]): ValidatedNel[FieldError, String] = {
    System.out.println("=====PPP PPP PPP===" + path + "==="+ so + "===" + label)

    val fieldName = displayName.map(n => s"'$n'").getOrElse("Field")
    denormal(so) match {
      //case None | Some("") => FieldError(path, s"$fieldName cannot be empty").invalidNel
      case Some("") => FieldError(path, s"'${label.getOrElse("Field")}' cannot be empty").invalidNel
      case Some(n) => n.validNel
      //case None => "".validNel //Todo:- removing this check for 'Lowcarbon heating', it might cause issue in BEIS forms..test BEIS forms
      case None => {
        System.out.println("===== None PPP PPP PPP===" + path + "==="+ so + "===" + label)

        FieldError(path, s"'${label.getOrElse("Field")}' cannot be empty").invalidNel
      }
    }
  }
}
