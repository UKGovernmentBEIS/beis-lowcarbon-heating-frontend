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

package services

import config.Config
import eu.timepit.refined.auto._
import models.{OppSectionNumber, OpportunityId}
import org.scalatest.{Matchers, WordSpecLike}

class UserURLsTest extends WordSpecLike with Matchers {
  //val baseUrl = Config.config.business.baseUrl

  val urls = new UserURLs("")

  "UserURLs" should {

    "generate correct url for login" in {
      urls.register shouldBe "/user/register"
    }

    "generate correct url for register" in {
      urls.login shouldBe "/user/login"
    }

    "generate correct url for forgotpassword" in {
      urls.forgotpassword shouldBe "/user/forgotpassword"
    }

    "generate correct url for resetpassword" in {
      urls.resetpassword shouldBe "/user/resetpassword"
    }
  }

}
