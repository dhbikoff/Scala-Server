import java.io.{File, FileInputStream, IOException, OutputStream}
import java.net.{ServerSocket, Socket, SocketException}
import scala.io.{BufferedSource, Codec}

object SimpleServer {
  def main(args: Array[String]) = {
    val serverSocket = new ServerSocket(8000)
    while(true) {
      val clientSocket = serverSocket.accept
      val response = new Thread(new ConResponse(clientSocket))
      response.start()
    }
    serverSocket.close
  }
}  

class ConResponse(clientSocket: Socket) extends Runnable {    
  case class ResponseHeader(
    status: String, 
    date: String = "Date: " + new java.util.Date,
    server: String = "Server: SSWS/0.01 (Simple Scala Web Server)",
    content: String = "Content-Type: text/html; charset=UTF-8", 
    connection: String = "Connection: close",
    empty: String = "") {

    def toList: List[String] = List(status, date, server, content, connection, empty)
    override def toString = this.toList.map( x => x + "\r\n" ).mkString
    def getBytes: Array[Byte] = this.toString.getBytes
  } 

  def contentType(path: String): String = {
    val ext = (path split ('.')).last
    val content = "Content-Type: "
    val typeName = ext match {
      case "css" => "text/" + ext
      case "js" => "text/javascript"
      case "gif" | "png" => "image/" + ext
      case "jpg" | "jpeg" => "image/jpeg"
      case "pdf" => "application/" + ext
      case "mp4" | "avi" | "mov" | "m4v" | 
        "ogv" | "flv" | "webm" | "asf" => "video/" + ext
      case "txt" => "text/plain"
      case _ => "text/html; charset=UTF-8"
    }
    content + typeName
  }

  def buildHeader(fileName: String, found: Boolean): ResponseHeader = {
    val header = {
      if (found) {
        val ctype = contentType(fileName) 
        ResponseHeader(status = "HTTP/1.0 200 OK", content = ctype)
      } else {
        ResponseHeader(status = "HTTP/1.0 404 Not Found") 
      }
    }
    header
  }

  def router(input: String): String = {
    val split = input.split(" ")
    val filename = if (split.length > 1) split(1) else "/"
    if (filename == "/") "/index.html"
    else filename
  }

  def buildResponse(input: String): (ResponseHeader, BufferedSource) = {
    val filename = router(input)
    val path = "public/" + filename
    val found = (new File(path)).isFile
    val header = buildHeader(filename, found)
    val body = {
      if (found) {
        // 200 OK
        val in = new FileInputStream(path)
        new BufferedSource(in)(Codec.ISO8859)
      }
      else {
        // 404 NOT FOUND
        val in = new FileInputStream("public/error.html")
        new BufferedSource(in)(Codec.ISO8859)
      }
    }
    (header, body)
  }

  def sendResponseBody(fileStream: BufferedSource, outStream: OutputStream) = {
      try {
        fileStream foreach { outStream write _ }
      } catch {
        case e: SocketException => {}
        case e: IOException => {}
      }
  }

  def run() { 
    val in = clientSocket.getInputStream
    while(in.available < 1) {} // wait for request

    // read request
    val buffer = new Array[Byte](in.available)
    in.read(buffer)
    val request = new String(buffer)
    println("-------REQUEST--------\n" + request)

    // build response
    val response = buildResponse(request)
    val header = response._1
    val body = response._2
    println("-------RESPONSE-------\n" + header.toString)

    // send response
    val out = clientSocket.getOutputStream
    out.write(header.getBytes)
    sendResponseBody(body, out)
    clientSocket.close() // closes in and out streams
  }
}

