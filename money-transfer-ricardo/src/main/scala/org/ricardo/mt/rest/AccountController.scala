package org.ricardo.mt.rest

import io.netty.handler.codec.http.HttpResponseStatus._
import org.ricardo.mt.http.path._
import org.ricardo.mt.http.swagger.{ApiError, ApiRequest, RestController}
import org.ricardo.mt.service._
import org.ricardo.mt.util.Money

import scala.concurrent.ExecutionContext


class AccountController(accountService: AccountService, transactionService: TransactionService)
                       (implicit protected val execctx: ExecutionContext) extends RestController {

  GET /"account" will "List all accounts" := { req: ApiRequest[Unit] =>
    accountService.getAccounts()
  }

  POST /"account" will "Create new Account" := { req: ApiRequest[Profile] =>
    accountService.addAccount(req.data)
  }

  GET /"account"/int("id") will "Get account by id" := { req: ApiRequest[Unit] =>
    val id = req.param[Int]("id")
    accountService.getAccount(id).map(_.getOrElse {
      throw new NotFoundException("account", id)
    })
  } errors (
    NOT_FOUND -> "Account is not found"
  )

  GET /"account"/int("id")/"transaction" will "List account transactions" := { req: ApiRequest[Unit] =>
    val id = req.param[Int]("id")
    transactionService.getTransactions(id).map(_.map(TransactionView(_)))
  } errors (
    NOT_FOUND -> "Account is not found"
  )

  POST /"account"/int("id")/"transaction"/"credit" will "Add money to account" := { req: ApiRequest[Credit] =>
    transactionService.creditAccount(req.param[Int]("id"), Money(req.data.value)).transform(_ => (), {
      case e: IllegalArgumentException => new BadRequest(e.getMessage)
      case e => e
    })
  } errors (
    NOT_FOUND -> "Account is not found",
    BAD_REQUEST -> "Wrong parameters"
  )

  POST /"account"/int("id")/"transaction"/"transfer" will "Add money to account" := { req: ApiRequest[Transfer] =>
    transactionService.transferToAccount(req.param[Int]("id"), req.data.to, Money(req.data.value)).transform(_ => (), {
      case e: IllegalArgumentException => new BadRequest(e.getMessage)
      case e => e
    })
  } errors (
    NOT_FOUND -> "Account is not found",
    BAD_REQUEST -> "Wrong parameters"
  )
}

class NotFoundException(resource: String, id: Any) extends ApiError(NOT_FOUND, s"Resource $resource#$id is not found")
class BadRequest(message: String) extends ApiError(BAD_REQUEST, message)

case class Credit(value: BigDecimal)
case class Transfer(value: BigDecimal, to: Int)

case class TransactionView(id: Int, time: Long, amount: BigDecimal, accountId: Int, status: String, comment: String)
object TransactionView {
  def apply(t: Transaction): TransactionView = apply(
    id = t.info.id,
    time = t.info.time,
    amount = t.info.amount.toDecimal,
    accountId = t.info.accountId,
    status = t.info.status.getClass.getSimpleName.stripSuffix("$"),
    comment = t match {
      case t: CreditTransaction =>
        "Credited by the GOD!"
      case t: TransferFromTransaction =>
        s"Money transfer to account #${t.toAccount}"
      case t: TransferToTransaction =>
        s"Money transfer from #${t.fromAccount} ref #${t.fromTransaction}"
    }
  )
}
