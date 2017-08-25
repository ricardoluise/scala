package org.ricardo.mt.service.memory

import java.util.concurrent.atomic.AtomicInteger

import org.ricardo.mt.service._
import org.ricardo.mt.util.Money

import scala.concurrent.{ExecutionContext, Future}


class MemoryTransferService(implicit execctx: ExecutionContext) extends AccountService with TransactionService {

  private val storage = new AccountStorage()

  private val accountIdSeq = new AtomicInteger(0)
  private val transactionIdSeq = new AtomicInteger(0)

  def addAccount(profile: Profile): Future[Account] = Future {
    val id = accountIdSeq.incrementAndGet()
    val rec = new AccountRecord(name = profile.name)
    storage.add(id, rec)
    Account(id, profile, Money(rec.balance))
  }

  def getAccounts(): Future[Seq[Account]] = Future {
    storage.iterator.map { case (id, rec) =>
      Account(id, Profile(rec.name), Money(rec.balance))
    }.toSeq.sortBy(_.id)
  }

  def getAccount(id: Int): Future[Option[Account]] = Future {
    storage.get(id).map(rec => Account(id, Profile(rec.name), Money(rec.balance)))
  }

  def getTransactions(accountId: Int): Future[Seq[Transaction]] = Future {
    storage.get(accountId) match {
      case None =>
        throw new NoSuchElementException(s"Unknown account $accountId")

      case Some(acc) =>
        acc.synchronized {
          acc.transactions.toIndexedSeq.map(_.toTransaction(accountId))
        }
    }
  }

  def creditAccount(accountId: Int, amount: Money): Future[CreditTransaction] = Future {
    if (amount.value < 0) {
      throw new IllegalArgumentException("Amount < 0")
    }
    storage.get(accountId) match {
      case None =>
        throw new NoSuchElementException(s"Unknown account $accountId")

      case Some(acc) =>
        acc.synchronized {
          val tr = transaction(acc, amount.value, TransactionType.Credit())
          tr.status = TransactionStatus.COMPLETE

          tr.toTransaction(accountId)
        }
    }
  }

  def transferToAccount(fromAccountId: Int, toAccountId: Int, amount: Money): Future[(TransferFromTransaction, TransferToTransaction)] = Future {
    if (amount.value < 0) {
      throw new IllegalArgumentException(s"Amount $amount < 0")
    }
    if (fromAccountId == toAccountId) {
      throw new IllegalArgumentException("Accounts should be different")
    }

    storage.get(fromAccountId) match {
      case None =>
        throw new NoSuchElementException(s"Unknown account $fromAccountId")

      case Some(fromAcc) =>
        storage.get(toAccountId) match {
          case None =>
            throw new NoSuchElementException(s"Unknown account $toAccountId")

          case Some(toAcc) =>
            val fromTr = fromAcc.synchronized {
              if (fromAcc.balance < amount.value) {
                throw new IllegalArgumentException("Not enough money")
              }

              transaction(fromAcc, -amount.value, TransactionType.TransferFrom(toAccountId))
            }

            val toTr = toAcc.synchronized {
              val tr = transaction(toAcc, amount.value, TransactionType.TransferTo(fromAccountId, fromTr.id))
              fromTr.status = TransactionStatus.COMPLETE
              tr
            }

            (fromTr.toTransaction(fromAccountId), toTr.toTransaction(toAccountId))
        }
    }
  }

  private def transaction[T <: TransactionType](account: AccountRecord, amount: Long, ttype: T): TransactionRecord[T] = {
    val tr = new TransactionRecord(
      id = transactionIdSeq.incrementAndGet(),
      amount = amount,
      time = System.currentTimeMillis(),
      ttype = ttype
    )
    account.transactions += tr
    account.balance += amount
    tr
  }
}

