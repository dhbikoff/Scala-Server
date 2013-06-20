import java.io.{BufferedInputStream, BufferedOutputStream, PrintStream}
import java.net.ServerSocket

object server {
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
      case "/" => input + "\n" + (new java.util.Date)
      case _   => "<HTML><h1>404 NOT FOUND</h1></h1>"
    }
  }
}
