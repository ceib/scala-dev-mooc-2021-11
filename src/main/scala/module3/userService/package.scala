package module3

import zio.{Has, RIO}
import zio.macros.accessible
import emailService.Email
import emailService.EmailAddress
import emailService.Html
import zio.console
import zio.ZLayer
import zio.ZIO
import module3.emailService.EmailService
import module3.userDAO.UserDAO

package object userService {

    type UserService = Has[UserService.Service]

    @accessible
    object UserService{

      trait Service{
          def notifyUser(userId: UserID): RIO[EmailService with console.Console, Unit]
      }

      class ServiceImpl(userDAO: UserDAO.Service) extends Service{
          def notifyUser(userId: UserID): RIO[EmailService with zio.console.Console,Unit] = 
              for{
                user <- userDAO.findBy(userId).some.mapError(_ => new Throwable(""))
                email = Email(user.email, Html("Hello here"))
                _ <- EmailService.sendMail(email)
              } yield ()
      }

      val live: ZLayer[UserDAO, Nothing, UserService] = ZLayer.fromService[UserDAO.Service, UserService.Service]( dao => 
          new ServiceImpl(dao)
          )

    } 

}
