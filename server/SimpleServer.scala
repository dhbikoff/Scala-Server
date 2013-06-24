import scala.io._
import java.io._
import java.net.ServerSocket

object SimpleServer {

  case class ResponseHeader(status: String, 
      date: String = "Date: " + new java.util.Date,
      server: String = "Server: SSWS/0.01 (Simple Scala Web Server)",
      content: String = "Content-Type: text/html; charset=UTF-8", 
      connection: String = "Connection: close",
      empty: String = "\r\n") {
  
    def toList: List[String] = List(status, date, server, content, connection, empty)
    override def toString = this.toList.map( x => x + "\r\n" ).mkString
    def getBytes: Array[Byte] = this.toString.getBytes
  } 

  case class Response(header: ResponseHeader, body: Array[Byte]) {
    override def toString = header.toString + body
    def getBytes = header.getBytes ++ body
  }

  def fileToBytes(filePath: String): Array[Byte] = { 
    val file = new File(filePath)    
    if (file.exists && file.isFile) {
      val src = Source.fromFile(file)(Codec.ISO8859)  
      (src map { ch => ch.toByte }).toArray
    }
    else null
  }

  def contentType(ext: String): String = ext match {
    case "js" | "css" => "Content-Type: text/" + ext
    case "gif" | "png" | "jpg" => "Content-Type: image/" + ext
    case "pdf" => "Content-Type: application/pdf"
    case _ => "Content-Type: text/html; charset=UTF-8"
  }

  def response(file: String): Array[Byte] = {
    val extension = {
      if (!file.contains('.')) "html; charset=UTF-8"
      else (file split ('.')).last
    }

    val body = fileToBytes("public" + file)
    if (body != null) {
      val ctype = contentType(extension) 
      val header =  ResponseHeader(status = "HTTP/1.0 200 OK", 
                    content = ctype)
      println("-------RESPONSE-------\n" + header.toString)
      Response(header, body).getBytes
    } else {
      val header = ResponseHeader("HTTP/1.0 404 Not Found")
      println("-------RESPONSE-------\n" + header.toString)
      Response(header, "<HTML><h1>404 NOT FOUND</h1></h1>".getBytes).getBytes
    }
  }

  def router(input: String): Array[Byte] = {
    val route = {
      val s = input.split(" ")(1)
      if (s == "/") "/index.html"
      else s
    }
    response(route)
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
      val bytes = output
      outputStream.write(output)
      outputStream.flush
      clientSocket.close
    }
    serverSocket.close
  }
}
