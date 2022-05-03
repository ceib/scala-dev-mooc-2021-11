package module3

import zio.Has
import zio.{URIO, UIO}
import zio.{ZLayer, ULayer}
import zio.console
import zio.ZIO
import zio.macros.accessible


package object emailService {

    type EmailService = Has[EmailService.Service]

    @accessible
    object EmailService{
        trait Service{
            def sendMail(email: Email): URIO[zio.console.Console, Unit]
        }

        val live: ULayer[EmailService] = ZLayer.succeed(new Service {
            def sendMail(email: Email): URIO[zio.console.Console,Unit] = 
                zio.console.putStrLn(email.toString()).orDie
        })
    }

}
