package module4.phoneBook.services

import zio.Has
import zio.Task
import module4.phoneBook.dto._
import zio.{ZLayer, ULayer}
import zio.ZIO
import zio.RIO
import zio.IO
import zio.macros.accessible
import java.sql.DriverManager
import module4.phoneBook.dao.repositories.{PhoneRecordRepository, AddressRepository}
import module4.phoneBook.db.DataSource
import module4.phoneBook.db
import zio.interop.catz._
import module4.phoneBook.dao.entities.PhoneRecord
import zio.random.Random
import io.getquill.{CompositeNamingStrategy2, Escape, Literal}
import module4.phoneBook.dao.entities.Address

@accessible
object PhoneBookService {

     type PhoneBookService = Has[Service]
  
     trait Service{
       def find(phone: String): ZIO[DataSource, Option[Throwable], (String, PhoneRecordDTO)]
       def insert(phoneRecord: PhoneRecordDTO): RIO[DataSource with Random, String]
       def update(id: String, addressId: String, phoneRecord: PhoneRecordDTO): RIO[DataSource, Unit]
       def delete(id: String): RIO[DataSource, Unit]
     }

    class Impl(phoneRecordRepository: PhoneRecordRepository.Service, addressRepository: AddressRepository.Service) extends Service {
       val ctx  = db.Ctx
       import ctx._
        def find(phone: String): ZIO[DataSource, Option[Throwable], (String, PhoneRecordDTO)] = for{
          result <- phoneRecordRepository.find(phone).some
        } yield (result.id, PhoneRecordDTO.from(result))

        def insert(phoneRecord: PhoneRecordDTO): RIO[DataSource with Random, String] = for{
          uuid <- zio.random.nextUUID.map(_.toString())
          uuid2 <- zio.random.nextUUID.map(_.toString())
          address = Address(uuid, phoneRecord.zipCode, phoneRecord.address)
          _ <- ctx.transaction(
                  for{
                    _ <- addressRepository.insert(address)
                    _ <- phoneRecordRepository.insert(PhoneRecord(uuid2, phoneRecord.phone, phoneRecord.fio, address.id))
                  } yield ()
                )
        } yield uuid
        
        def update(id: String, addressId: String,  phoneRecord: PhoneRecordDTO): RIO[DataSource, Unit] = for{
            _ <- phoneRecordRepository.update(PhoneRecord(id, phoneRecord.phone, phoneRecord.fio, addressId))
        } yield ()
        
        def delete(id: String): RIO[DataSource, Unit] = for{
            _ <- phoneRecordRepository.delete(id)
        } yield ()
        
    }

    val live: ZLayer[PhoneRecordRepository.PhoneRecordRepository with AddressRepository.AddressRepository, Nothing, PhoneBookService.PhoneBookService] = 
      ZLayer.fromServices[PhoneRecordRepository.Service, AddressRepository.Service, PhoneBookService.Service]((repo, addressRepo) => 
        new Impl(repo, addressRepo)
      )

}