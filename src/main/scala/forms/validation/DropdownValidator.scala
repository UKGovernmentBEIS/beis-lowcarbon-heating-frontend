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
import play.api.i18n.{Messages, MessagesApi}
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

import scala.util.Try

case class DropdownValidator(label: Option[String] = None) extends FieldValidator[Option[String], String] {

  val msg = controllers.GlobalContext.injector.instanceOf[MessagesApi]

  override def normalise(os: Option[String]): Option[String] = os.map(_.trim())

  override def doValidation(path: String, s: Normalised[Option[String]]): ValidatedNel[FieldError, String] = {
    denormal(s) match {
      case Some(fld) if fld.equals("Select one") =>
        FieldError(path, msg("error.BF011", s"'${label.getOrElse("Field")}'")  ).invalidNel
      case _ => "".validNel
    }
  }
}
