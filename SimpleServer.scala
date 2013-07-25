import scala.io._
import java.io._
import java.net._

object SimpleServer {

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
    val ext = {
      if (!path.contains('.')) "html; charset=UTF-8"
      else (path split ('.')).last
    }

    val ct = "Content-Type: "
    val typeName = ext match {
      case "js" | "css" => "text/" + ext
      case "gif" | "png" | "jpg" => "image/" + ext
      case "pdf" => "application/" + ext
      case "mp4" => "video/" + ext
      case _ => "text/html; charset=UTF-8"
    }
    ct + typeName
  }

  def header(fileName: String, found: Boolean): ResponseHeader = {
    val header = {
      if (found) {
        val ctype = contentType(fileName) 
        ResponseHeader(status = "HTTP/1.0 200 OK", content = ctype)
      } else {
        ResponseHeader(status = "HTTP/1.0 404 Not Found") 
      }
    }
    println("-------RESPONSE-------\n" + header.toString)
    header
  }

  def router(input: String): (ResponseHeader, Option[FileInputStream]) = {
    val route = {
      val s = input.split(" ")(1)
      if (s == "/") "/index.html"
      else s
    }
    response(route)
  }

  def response(filename: String) = {
    val path = "public/" + filename
    val found = new File(path).exists
    val head = header(filename, found)
    val src = {
      if (found)
        Some(new FileInputStream(path))
      else None
    }
    (head, src)
  }

  implicit class InputStreamForeach(in: FileInputStream) {
    def foreach(f: Array[Byte] => Unit): Boolean = {
      val arr = new Array[Byte](4096)
      var available = 1
      while (available > 0 ) {
        available = in.read(arr, 0, 4096)
        if (available > 0) {
          try {
            f(arr)
          } catch {
            case e: SocketException => {
              println("connection closed")
              available = -1
              return true
          }
            case e: Exception => {
              println(e.toString)
              available = -1
              return true
            }
          }        
        }
      }
      false
    }  
  }

  def main(args: Array[String]) = {
    val serverSocket = new ServerSocket(8000)
    while(true) {
      val clientSocket = serverSocket.accept
      val inputStream = new BufferedInputStream(clientSocket.getInputStream)
      val outputStream = new BufferedOutputStream(clientSocket.getOutputStream)

      while(inputStream.available < 1) {} 

      val buffer = new Array[Byte](inputStream.available)
      inputStream.read(buffer)
      val input = new String(buffer)
      println("-------REQUEST--------\n" + input)
      
      val output = router(input)
      val header = output._1
      val fileStream = output._2
      println("-------RESPONSE-------\n" + header.toString)
      outputStream.write(header.getBytes)

      val error = fileStream match {
        case Some(fs) => fs foreach (outputStream.write(_))
        case None => false 
      }

      if (!error) outputStream.flush
      clientSocket.close
      
   } 
    serverSocket.close
  }
}
