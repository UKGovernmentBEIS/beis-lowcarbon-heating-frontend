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

package pdf

/**
  * Created by venkatamutyala on 25/01/2018.
  */
import java.io.{ByteArrayOutputStream, StringReader, StringWriter}

import org.w3c.tidy.Tidy
import org.xhtmlrenderer.pdf.ITextRenderer
import org.xhtmlrenderer.resource.XMLResource
import org.xhtmlrenderer.context.StyleReference

/**
  *
  * `PdfRenderer`
  *
  * a simple wrapper to generate content that could be rendered as pdf
  *
  * @param classLoader:ClassLoader - The class loader used to resolve assets
  * @param customRenderer:ITextRenderer - custom renderer to do changes on the fly
  */
class PdfRenderer(classLoader: ClassLoader, customRenderer: ITextRenderer = new ITextRenderer) {

  private val renderer = doto(customRenderer) { renderer =>
    // spaghetti with bolognese
    val sharedContext = renderer.getSharedContext
    val userAgent = new ClassLoaderUserAgent(
      renderer.getOutputDevice,
      classLoader,
      sharedContext
    )
    sharedContext.setUserAgentCallback(userAgent)
    sharedContext.setCss(new StyleReference(userAgent))
  }

  private val tidy = doto(new Tidy)(_ setXHTML true)

  def toBytes(body: String): Array[Byte] =
    toStream(body).toByteArray

  def toStream(body: String): ByteArrayOutputStream =
    doto(new ByteArrayOutputStream) { output =>
      try {
        val reader = new StringReader(tidify(body))
        val document = XMLResource.load(reader).getDocument
        renderer.setDocument(document, "")
        renderer.layout()
        renderer.createPDF(output)
      } finally output.close()
    }

  private def tidify(body: String) =
    doto(new StringWriter) {
      tidy.parse(new StringReader(body), _)
    }.toString

  private def doto[T](t: T)(code: T => Unit): T = {code(t); t}
}
