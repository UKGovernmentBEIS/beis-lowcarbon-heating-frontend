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

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import models.MessageId
import org.scalatest.{Matchers, WordSpecLike}

class MessageBoardURLsTest extends WordSpecLike with Matchers {

  val i: Long Refined Positive = 1L

  val urls = new  MessageBoardURLs("")

  "MessageBoardURLs" should {

    "generate correct url for messageboard home" in {
      urls.messages shouldBe "/messageboard"
    }

    "generate correct url for get message by Id" in {
      urls.messagesById(MessageId(i)) shouldBe "/message/1/messageboard"
    }

    "generate correct url for get message by Id" in {
      urls.delete(MessageId(i)) shouldBe "/message/1/delete"
    }
  }

}
