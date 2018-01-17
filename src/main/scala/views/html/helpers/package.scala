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

package views.html

import javax.inject.Inject

import config.Config
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import forms.{AddressField, TextAreaField, TextField}
import models.{AppAuthPayload, ApplicationForm, NonEmptyString}
import play.api.Play
import play.api.libs.json.Json
import play.twirl.api.Html
import services.JWTOps

package object helpers {
  def textForQuestions(appForm: ApplicationForm): Html = {
    val constraints = appForm.sections.flatMap { s =>
      s.questions.flatMap { q =>
        s.fields.find(_.name == q.key).flatMap {
          case TextField(_, _, _, _, _, wordCount) => Some(s"Word count: $wordCount")
          case TextAreaField(_, _, _, _, wordCount) => Some(s"Word count: $wordCount")
          case AddressField(_, _, _, _, wordCount) => Some(s"Word count: $wordCount")
          case _ => None
        }.map(constraintText => (q.key, constraintText))
      }
    }
    val descriptions = appForm.sections.flatMap { s =>
      s.questions.flatMap { q =>
        q.description.map(q.key -> _.value)
      }
    }

    views.html.partials.oppQuestionsSection(appForm, Map(constraints: _*), Map(descriptions: _*))
  }

  def formatId(id: Long): String = f"BEIS $id%04d"

  def formatId(id: Long Refined Positive): String = formatId(id.value)

  def splitLines(s: String): Seq[String] = s.split("\n")

  def splitLines(s: Option[NonEmptyString]): Seq[String] = s.map(nes=>splitLines(nes.value)).getOrElse(List[String]())

  def splitLineswithR(s: String): List[String] = s.split("""\\r\\n""").toList.map(_.replaceAll("""\\""", "")).toList

  private def instance = Play.current.injector.instanceOf[JWT]

  implicit val appAuthPayloadWrites = Json.writes[AppAuthPayload]

  def JWTToken(grpId: String, userId: String, appId: String) = {
    instance.getJWTToken(grpId, userId, appId)
  }
}

/**  ::: JWT:::
  * This is other way of creating JWT Token
  * if JWT token is required directly in HTML page.
  * eg.. fileUpload.html (to download a AWS file)
**/

case class JWT  @Inject()(jwt: JWTOps ) {
  implicit val appAuthPayloadWrites = Json.writes[AppAuthPayload]

  def getJWTToken(grpId: String, userId: String, appId: String) = {
    val appAuthpayload =  Json.toJson(AppAuthPayload(grpId, userId, appId.toString)).toString()
    val appAuthToken = jwt.createToken(appAuthpayload)
    val appFrontEndUrlWithJWTToken = s"$appAuthToken"
    appFrontEndUrlWithJWTToken
  }
}