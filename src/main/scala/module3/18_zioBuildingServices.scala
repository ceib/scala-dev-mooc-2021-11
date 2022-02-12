package module3


import zio.{Has, ZIO}
import zio.ZLayer
import zio.console.Console
import zio.URIO
import module3.userService.UserService
import module3.userService.UserID
import module3.emailService.EmailService
import module3.userDAO.UserDAO

object buildingZIOServices{

  lazy val app: ZIO[UserService with EmailService with Console, Throwable, Unit] = 
    UserService.notifyUser(UserID(1))
  lazy val appEnv: ZLayer[Any, Nothing, UserService with EmailService] = 
    UserDAO.live >>> UserService.live ++ EmailService.live

  

  def main(args: Array[String]): Unit = {
     zio.Runtime.default.unsafeRun(app.provideSomeLayer[zio.console.Console](appEnv))
  }

}