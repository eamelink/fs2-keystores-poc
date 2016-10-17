import fs2._
import fs2.{io, text, Task}
import scala.io.StdIn

object MyApp extends App {
  implicit val strategy = Strategy.fromExecutionContext(scala.concurrent.ExecutionContext.global)

  type Keystore = String
  type Password = String

  def keystores = Stream[Nothing, Keystore]("Amsterdam", "Rotterdam", "Amersfoort", "Alkmaar", "Utrecht", "Urk")

  val keystoresWithPassword = keystores.pull { handle =>
    // This pull emits pairs of Keystore and Password
    // The resource in the loop is a pair of the next handle, and the accumulated passwords
    Pull.loop[Task, (Keystore, Password), (Handle[Task, Keystore], Set[Password])] { case (handle, passwords) =>
      for {
        (keystore, newHandle) <- handle.await1
        password <- getPasswordFor(keystore, passwords)
        _ <- Pull.output1((keystore, password))
      } yield (newHandle, (passwords + password))
    }((handle, Set.empty))
  }

  // Exercise for the reader; deal with wrong passwords
  def getPasswordFor(keystore: String, passwords: Set[String]): Pull[Task, Nothing, String] = {
    passwords.find { elem => keystore.startsWith(elem) } match {
      case Some(existingPassword) => Pull.pure(existingPassword)
      case None =>
        Pull.eval {
          Task {
            println(s"Please enter your password for keystore '$keystore'")
            StdIn.readLine()
          }
        }
      }
  }

  println(keystoresWithPassword.runLog.unsafeRun)

}
