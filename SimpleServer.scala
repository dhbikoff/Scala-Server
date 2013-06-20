import java.io.{BufferedInputStream, BufferedOutputStream, PrintStream}
import java.net.ServerSocket

object SimpleServer {

  case class ResponseHeader(status: String, 
      date: String = "Date: " + new java.util.Date,
      server: String = "Server: CSWS/0.01 (Crappy Scala Web Server)",
      content_type: String = "Content-Type: text/html; charset=UTF-8", 
      connection: String = "Connection: close",
      empty: String = "") {
  
    def toList: List[String] = List(status, date, server, content_type, connection, empty)
    override def toString = this.toList.map( x => x + "\r\n" ).mkString
  } 

  def main(args: Array[String]) = {
    val serverSocket = new ServerSocket(8000)
    while(true) {
      val clientSocket = serverSocket.accept
      val inputStream = new BufferedInputStream(clientSocket.getInputStream)
      val outputStream = new PrintStream(new BufferedOutputStream(clientSocket.getOutputStream))

      while(inputStream.available < 1) { Thread.sleep(1000) } 

      val buffer = new Array[Byte](inputStream.available)
      inputStream.read(buffer)
      val input = new String(buffer)
      println(input)
      val output = router(input)
      outputStream.print(output)
      outputStream.flush
      clientSocket.close
    }
    serverSocket.close
  }

  def router(input: String):String = {
    val split = input.split(" ")
    split(1) match {
      case "/" => {
        val r = ResponseHeader("HTTP/1.0 200 OK").toString
        r + r
      }
      case _   => ResponseHeader("HTTP/1.0 404 Not Found") + "<HTML><h1>404 NOT FOUND</h1></h1>"
    }
  }
}
