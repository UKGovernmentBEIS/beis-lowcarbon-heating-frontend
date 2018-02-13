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

package controllers

import java.io._
import java.net._
import java.util.zip.{ZipEntry, ZipOutputStream}
import javax.inject.Inject

import akka.stream.scaladsl.StreamConverters
import com.amazonaws.services.s3.model.{ObjectMetadata, PutObjectRequest}
import config.Config
import forms.Field
import models.{AppAuthPayload, AppSectionNumber, ApplicationForm, ApplicationId, ResourceKey}
import org.joda.time.DateTime
import org.w3c.dom.Document
import org.w3c.tidy.Tidy
import org.xhtmlrenderer.pdf.ITextRenderer
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import services.{AWSOps, ApplicationOps, JWTOps}
import views.html.helpers._

import scala.concurrent.{ExecutionContext, Future}


/**
  * Created by venkatamutyala on 25/01/2018.
  */

class PDFController @Inject()(actionHandler: ActionHandler,
                              awsHandler: AWSHandler,
                              awsS3: AWSOps,
                              applications: ApplicationOps,
                              jwt: JWTOps)(implicit ec: ExecutionContext) extends Controller {

  implicit val fileAttachmentR = Json.reads[FileAttachment]
  val appFrontEndUrl = Config.config.business.appFrontEndUrl
  val dateTime = new DateTime()
  val exp = Config.config.jwt.exp
  val expiry = new DateTime((dateTime.getMillis + exp.toLong)).getMillis

  def applicationPreviewPDF(id: ApplicationId) = Action.async { implicit request =>

    val mp = request.queryString
    val token =  actionHandler.getValueFromRequest("token", mp )

    /*** Get Application Object and Create a PDF using iText & Flying Saucer API **/

    val appAuthpayload =  Json.toJson(AppAuthPayload("", "", id.id.toString, expiry.toLong)).toString()
    //val appAuthToken = jwt.createToken(appAuthpayload)
    val url = s"$appFrontEndUrl/simplepreview/application/${id.id}/content/pdf?token=$token"
    val appURL:URL = new URL(url)
    val renderer:ITextRenderer = new ITextRenderer()
    val tidy: Tidy = new Tidy()
    tidy.setAsciiChars(true)
    tidy.setBreakBeforeBR(true)
    tidy.setTidyMark(false)
    tidy.setDocType("omit")
    tidy.setXHTML(true)
    tidy.setInputEncoding("utf-8"); //to correct £ symbol characters
    val doc:Document = tidy.parseDOM(appURL.openStream(), null)

    renderer.setDocument(doc, null)
    renderer.layout()
    val baos:ByteArrayOutputStream = new ByteArrayOutputStream()
    renderer.createPDF(baos)
    val input:ByteArrayInputStream = new ByteArrayInputStream(baos.toByteArray)
    /*** Upload to AWS S3 **/
    awsS3.uploadStream(ResourceKey("LowcarbonHeating.pdf"), input)
    /*** Get Application PDF ends **/

    /*** Create ZIP file and add all items to it **/
    val preq = new PutObjectRequest("beis-forms-test-bucket", "LowcarbonHeating.pdf", input, new ObjectMetadata())
    input.close()
    baos.close()

    var baoss  = new ByteArrayOutputStream()
    var zos = new ZipOutputStream(new BufferedOutputStream(baoss))
    val keyLC = ResourceKey("LowcarbonHeating.pdf")

    for(
        urlLC <- awsS3.downloadDirect(keyLC);
        key_urls <- getAWSS3URLs(id)

    )yield {

        /** add Application PDF **/
        addToZip(keyLC, urlLC, zos)

        /** add attachements from AWS **/
        key_urls.map{ ku =>
          addToZip(ku._1, ku._2, zos)
        }

        zos.close()
        baoss.close()

        //Ok(baoss.toByteArray).as("application/zip")

        val inp: ByteArrayInputStream = new ByteArrayInputStream(baoss.toByteArray)
        Ok.chunked(StreamConverters.fromInputStream(() => inp)).withHeaders(
          CONTENT_TYPE -> "application/zip",
          CONTENT_DISPOSITION -> s"attachment; filename = LowcarbonHeatingFiles.zip"
        )
    }
  }

  def addToZip(key: ResourceKey, url: URL, zos: ZipOutputStream) = {

    zos.putNextEntry(new ZipEntry(key.key))
    var source = scala.io.Source.fromURL(url, "ISO-8859-1")
    var buffer: Array[Byte] = source.map(_.toByte).toArray
    zos.write(buffer)
    source.close()
    zos.closeEntry()
  }

  case class FileAttachment(itemNumber: Long, supportingDocuments: String)

  def getAWSS3URLs(id: ApplicationId):Future[Seq[(ResourceKey, URL)]] = {

    applications.detail(id).flatMap {
        case Some(app) =>
          val d = app.sections.flatMap { section =>

             /* Create List of ResourceKeys */
             if(section.answers.toString.contains("supportingDocuments")) {
                 val ls =  section.answers.value.flatMap{ m=>
                      m._2.validate[Seq[FileAttachment]].getOrElse(Seq()).map{v=>

                        ResourceKey(s"${v.itemNumber}.${v.supportingDocuments.split('.').last}")
                      }
                    }.toSeq
                 ls
                }
                else
                  Seq()
            }

          /* Create List of AWS S3 Presigned URs from List of ResourceKeys */
          awsS3.downloadDirectMultiFilesWithKeys(d)

        case _ =>
          val ur = new URL("")
          Future(Seq())
    }
  }

  def convertToAWSS3URL(fileName: String):Future[URL] =
    awsS3.downloadDirect(ResourceKey(fileName))


  def convertToAWSS3URL_(fileName: String, id: ApplicationId):String = {
    val appAuthpayload =  Json.toJson(AppAuthPayload("", "", id.id.toString, expiry.toLong)).toString()
    val appAuthToken = jwt.createToken(appAuthpayload)
    s"${appFrontEndUrl}/downloadfile/${fileName}?token=${appAuthToken}"
  }

  def getFilenames(id: ApplicationId):Future[Seq[String]] = {

  applications.detail(id).flatMap {
      case Some(app) =>
        val d = app.sections.flatMap { section =>
           if(section.answers.toString.contains("supportingDocuments")) {
               val ls =  section.answers.value.flatMap{ m=>
                    m._2.validate[Seq[FileAttachment]].getOrElse(Seq()).map{v=>
                       s"${v.itemNumber}.${v.supportingDocuments.split('.').last}"
                    }
                  }.toSeq
               ls
              }
              else
                Seq()
          }
        Future(d)

      case _ => Future(Seq())
    }
  }


  def getFieldMap(form: ApplicationForm): Map[AppSectionNumber, Seq[Field]] = {
    Map(form.sections.map(sec => sec.sectionNumber -> sec.fields): _*)
  }

}
