package module4.homework.dao.repository

import io.getquill.{Action, ActionReturning, EntityQuery, Insert, Quoted}
import zio.{Has, ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc.QIO
import zio.macros.accessible
import module4.homework.dao.entity.{Role, RoleCode, User, UserId, UserToRole}
import module4.phoneBook.db


object UserRepository{


    val dc = db.Ctx
    import dc._

    type UserRepository = Has[Service]

    trait Service{
        def findUser(userId: UserId): Result[Option[User]]
        def createUser(user: User): Result[User]
        def createUsers(users: List[User]): Result[List[User]]
        def updateUser(user: User): Result[Unit]
        def deleteUser(user: User): Result[Unit]
        def findByLastName(lastName: String): Result[List[User]]
        def list(): Result[List[User]]
        def userRoles(userId: UserId): Result[List[Role]]
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): Result[Unit]
        def listUsersWithRole(roleCode: RoleCode): Result[List[User]]
        def findRoleByCode(roleCode: RoleCode): Result[Option[Role]]
    }

    class ServiceImpl extends Service{

        val userSchema = quote{
            querySchema[User](""""User"""")
        }

        val roleSchema = quote{
            querySchema[Role](""""Role"""")
        }

        val userToRoleSchema = quote{
            querySchema[UserToRole](""""UserToRole"""")
        }

        def findUser(userId: UserId): Result[Option[User]] = run(userSchema.filter(_.id == lift(userId.id))).map(_.headOption)

        def createUser(user: User): Result[User] =
            run(userSchema
              .insert(lift(user))
              .onConflictIgnore
              .returning(user => user))

        def createUsers(users: List[User]): Result[List[User]] = ZIO.foreach(users)(createUser)

        def updateUser(user: User): Result[Unit] = run(userSchema.filter(_.id == lift(user.id)).update(lift(user))).unit

        def deleteUser(user: User): Result[Unit] = run(userSchema.filter(_.id == lift(user.id)).delete).unit

        def findByLastName(lastName: String): Result[List[User]] = run(userSchema.filter(_.lastName == lift(lastName)))

        def list(): Result[List[User]] = run(userSchema)

        def userRoles(userId: UserId): Result[List[Role]] =
            run(for {
                u2r <- userToRoleSchema.filter(_.userId == lift(userId.id))
                role <- roleSchema.join(_.code == u2r.roleId)
            } yield role)

        def insertRoleToUser(roleCode: RoleCode, userId: UserId): Result[Unit] =
            run(userToRoleSchema.insert(lift(UserToRole(roleCode.code, userId.id))).onConflictIgnore).unit

        def listUsersWithRole(roleCode: RoleCode): Result[List[User]] =
            run(for {
                u2r <- userToRoleSchema.filter(_.roleId == lift(roleCode.code))
                user <- userSchema.join(_.id == u2r.userId)
            } yield user)

        def findRoleByCode(roleCode: RoleCode): Result[Option[Role]] =
            run(roleSchema.filter(_.code == lift(roleCode.code))).map(_.headOption)

    }

    val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}