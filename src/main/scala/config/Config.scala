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

package config

case class BusinessConfig(baseUrl: String, bpmServerUrl: String, bpmServerContext: String,  emailto: String, addressSearch: String)
case class FileConfig(fileuploaddirectory: String, filedownloaddirectory: String, guidancedoc: String, allowedfileextensions: String,
                      allowedfilesize: Int)
case class AWSConfig(accesskey: String, secretkey: String, region: String, bucket: String, publicbucket: String, domain:String)
case class BPMConfig(bpmreqd: String, procuser: String, procpwd: String, procdefId: String)
case class FieldValidation(telephone: Int, email: Int, companyname: Int)
case class LoginConfig(sessionTimeout: Int, excludeSession: String)

case class Config(logAssets: Option[Boolean], logRequests: Boolean, prod: Boolean, business: BusinessConfig,
                  file: FileConfig, aws: AWSConfig, bpm: BPMConfig, fieldvalidation: FieldValidation, login: LoginConfig)

object Config {

  import pureconfig._

  lazy val config: Config = loadConfig[Config].get

}
