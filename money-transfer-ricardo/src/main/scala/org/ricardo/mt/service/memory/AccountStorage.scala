package org.ricardo.mt.service.memory

import java.util.concurrent.ConcurrentHashMap

import org.ricardo.mt.service._
import org.ricardo.mt.util.Money

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer


class AccountStorage {
  private val storage = new ConcurrentHashMap[Int, AccountRecord]().asScala

  def add(id: Int, rec: AccountRecord): Unit = {
    storage(id) = rec
  }

  def get(id: Int): Option[AccountRecord] = {
    storage.get(id)
  }

  def iterator: Iterator[(Int, AccountRecord)] = storage.iterator
}

class AccountRecord(var name: String) {
  val transactions = new ArrayBuffer[TransactionRecord[_]]()

  var balance = 0l

  def toAccount(id: Int) = Account(id, Profile(name), Money(balance))
}

//sealed trait TransactionRecord {
//  def props: TransactionProps
//}
//case class TransactionProps(id: Int, amount: Long, time: Long)

class TransactionRecord[T <: TransactionType](val id        : Int,
                                              val amount    : Long,
                                              val time      : Long,
                                              val ttype: T) {
  var status: TransactionStatus = TransactionStatus.HOLD

  def toTransaction(accountId: Int): ttype.SPEC = ttype.toTransaction(TransactionInfo(id, accountId, Money(amount), time, status))
}


sealed trait TransactionType {
  type SPEC <: Transaction
  def toTransaction(info: TransactionInfo): SPEC
}
object TransactionType {
  case class Credit() extends TransactionType {
    type SPEC = CreditTransaction
    def toTransaction(info: TransactionInfo) = CreditTransaction(info)
  }
  case class TransferFrom(toAccount: Int) extends TransactionType {
    type SPEC = TransferFromTransaction
    def toTransaction(info: TransactionInfo) = TransferFromTransaction(info, toAccount)
  }
  case class TransferTo(toAccount: Int, fromTransaction: Int) extends TransactionType {
    type SPEC = TransferToTransaction
    def toTransaction(info: TransactionInfo) = TransferToTransaction(info, toAccount, fromTransaction)
  }
}
