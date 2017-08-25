package org.ricardo.mt.rest

import org.ricardo.mt.service.{Account, Profile}
import org.ricardo.mt.util.Money
import play.api.libs.json._

trait JsonFormat {
  implicit val unitFormat = Format[Unit](
    Reads(_ => JsSuccess(Unit)),
    Writes(_ => JsNull)
  )

  implicit val moneyFormat = Format[Money](
    Reads {
      case JsNumber(d) => JsSuccess(Money(d))
      case unexpected => JsError(s"$unexpected is not a Number")
    },
    Writes { money =>
      JsNumber(money.toDecimal)
    }
  )
  implicit val profileFormat = Json.format[Profile]
  implicit val accountFormat = Json.format[Account]

  implicit val creditFormat = Json.format[Credit]
  implicit val transferFormat = Json.format[Transfer]

  implicit val transactionWrites = Json.writes[TransactionView]
}
