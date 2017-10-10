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
import forms.TextField
import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.{JsObject, JsString}

class ContactValidatorSpec extends WordSpecLike with Matchers {
  "Contact field validator" should {
    val email = TextField(Some("email"), "contact.email", true, true,  true, 10)
    val phone = TextField(Some("phone"), "contact.phone", true, true,  true, 10)
    val web = TextField(Some("web"), "contact.web", true, true,  true, 1)
    val twitter = TextField(Some("twitter"), "contact.twittter", true, true,  true, 1)
    val jsobj:JsObject = JsObject(Seq(
      "web" -> JsString("www.cc.com"),
      "email" -> JsString("test@csc.com"),
      "phone" -> JsString("02083242"),
      "twitter" -> JsString("t@csc.com")
    ))

    "reject a missing value of Email" in {

      ContactValidator(Seq(email, phone, web, twitter)).validate("contact", jsobj - "email").map{ e =>
      }.leftMap{err =>
        err.head.err shouldBe "'email' cannot be empty"
      } shouldBe  an[Invalid[_]]
    }

    "reject a length of Email exdeeds Specified number of characters" in {
       ContactValidator(Seq(email)).validate("contact", jsobj).map{ e =>
      }.leftMap{err =>
        err.head.err shouldBe "'email' Character limit exceeded"
      } shouldBe  an[Invalid[_]]
    }

    "approve an existing of Email" in {

      ContactValidator(Seq(email)).validate("contact", jsobj-"email" +
        ("email" -> JsString("t@csc.com"))).leftMap{ e =>
        fail("Character limit exceeded")
      }
    }

    "approve a length of Email that is under Specified number of characters" in {
       ContactValidator(Seq(email)).validate("contact", jsobj-"email" +
         ("email" -> JsString("t@csc.com"))) shouldBe an[Valid[_]]
    }

    "reject a missing value of Telephone" in {
      ContactValidator(Seq(phone)).validate("contact", jsobj - "phone").map{ e =>
      }.leftMap{err =>
        err.head.err shouldBe "'phone' cannot be empty"
      } shouldBe  an[Invalid[_]]
    }

    "reject a length of Telephone exdeeds Specified number of characters" in {
      val telephone = "123456789012345678901234567890"
      ContactValidator(Seq(phone)).validate("contact", jsobj-"phone" +
        ("phone" -> JsString(telephone))).leftMap{err =>
        err.head.err shouldBe "'phone' Character limit exceeded"
      } shouldBe  an[Invalid[_]]
    }

    "approve an existing of Telephone" in {
      val telephone = "12345678"
      ContactValidator(Seq(phone)).validate("contact", jsobj-"phone" +
        ("phone" -> JsString("12345"))).leftMap{ e =>
        fail("'phone' Character limit exceeded")
      }
    }

    "approve a length of Telephone that is inside Specified number of characters range" in {
      ContactValidator(Seq(phone)).validate("contact", jsobj) shouldBe an[Valid[_]]
    }

  }
}
