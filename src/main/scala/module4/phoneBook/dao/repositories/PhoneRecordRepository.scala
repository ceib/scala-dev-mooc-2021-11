package module4.phoneBook.dao.repositories

import module4.phoneBook.db
import zio.Has
import module4.phoneBook.dao.entities._
import io.getquill.CompositeNamingStrategy2
import io.getquill.Escape
import io.getquill.Literal
import zio.ZLayer
import zio.ULayer
import zio.Task
import io.getquill.context.ZioJdbc._
import io.getquill.Ord

object PhoneRecordRepository {
  val ctx = db.Ctx
  import ctx._

  type PhoneRecordRepository = Has[Service]

  trait Service{
      def find(phone: String): QIO[Option[PhoneRecord]]
      def list(): QIO[List[PhoneRecord]]
      def insert(phoneRecord: PhoneRecord): QIO[Unit]
      def update(phoneRecord: PhoneRecord): QIO[Unit]
      def delete(id: String): QIO[Unit]
  }

  class Impl extends Service{
     
     val phoneRecordSchema = quote{
       querySchema[PhoneRecord](""""PhoneRecord"""")
     }

     val addressSchema = quote{
       querySchema[Address](""""Address"""")
     }

    def find(phone: String): QIO[Option[PhoneRecord]] = 
      ctx.run(phoneRecordSchema.filter(_.phone == lift(phone)))
      .map(_.headOption)
    
    def list(): QIO[List[PhoneRecord]] = ctx.run(phoneRecordSchema)
    
    def insert(phoneRecord: PhoneRecord): QIO[Unit] = 
      ctx.run(phoneRecordSchema.insert(lift(phoneRecord))).unit
    
    def update(phoneRecord: PhoneRecord): QIO[Unit] = 
      ctx.run(phoneRecordSchema.filter(_.id == lift(phoneRecord.id))
      .update(lift(phoneRecord))).unit
    
    def delete(id: String): QIO[Unit] = 
      ctx.run(phoneRecordSchema.filter(_.id == lift(id))
      .delete).unit

      // implicit join

      def listWithAddress() = ctx.run(
        for{
           phoneRecord <- phoneRecordSchema
           address <- addressSchema if (phoneRecord.addressId == address.id)
        } yield (phoneRecord, address)
      )

      // applicative join
      def listWithAddress2() = ctx.run(
        phoneRecordSchema
        .join(addressSchema)
        .on(_.addressId == _.id)
        .filter(v => v._1.phone == lift(""))
      )

      // flat join
      def listWithAddress3() = ctx.run(
        for{   
          phoneRecord <- phoneRecordSchema
          address <- addressSchema.join(_.id == phoneRecord.addressId)
        } yield (phoneRecord, address)
      )

      private val q = quote(phoneRecordSchema.filter(_.phone == lift("1234")))

      def count = ctx.run(q.size)

      def paged = ctx.run(q.take(5).drop(20).sortBy(_.fio)(Ord.asc))
    
  }

 

  val live: ULayer[PhoneRecordRepository] = ZLayer.succeed(new Impl)
}
