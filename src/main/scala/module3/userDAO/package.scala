package module3

import zio.Has
import zio.Task
import userService.{User, UserID}
import zio.{ZLayer, ULayer}
import zio.macros.accessible

package object userDAO {

    type UserDAO = Has[UserDAO.Service]
    
    @accessible
    object UserDAO{
        trait Service{
            def list(): Task[List[User]]
            def findBy(id: UserID): Task[Option[User]]
           // def find[T](id: T): Task[Option[User]]
        }

        val live: ULayer[UserDAO] = ???
    }

  
}
