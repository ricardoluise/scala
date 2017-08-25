package org.ricardo.mt.rest

import com.ning.http.client.Response
import org.scalatest.FlatSpec
import play.api.libs.json._

import scala.reflect.Manifest


class TransferSpec extends FlatSpec with RestSpec {

  "Account" should "have empty list on start but works" in {
    val resp = request(_ / "account")
    resp.getStatusCode should equal (200)
    resp.getContentType should equal ("application/json")
    resp.getResponseBody should equal ("[]")
  }

  it should "be created with post" in {
    val resp = request(r => (r / "account").POST << """{"name": "test1"}""")
    resp.getStatusCode should equal (200)

    val acc = json[JsObject](resp)
    checkAccount(acc)(1, "test1", 0)
  }

  it should "have 1 item" in {
    val resp = request(_ / "account")
    resp.getStatusCode should equal (200)

    val accs = json[JsArray](resp)
    accs.value.size should equal (1)

    val acc = accs.value.head.asInstanceOf[JsObject]
    checkAccount(acc)(1, "test1", 0)
  }

  it should "have 2 items" in {
    val add = request(r => (r / "account").POST << """{"name": "test2"}""")
    add.getStatusCode should equal (200)

    val list = request(_ / "account")
    list.getStatusCode should equal (200)

    val accs = json[JsArray](list)
    accs.value.size should equal (2)

    checkAccount(accs.value(0).asInstanceOf[JsObject])(1, "test1", 0)
    checkAccount(accs.value(1).asInstanceOf[JsObject])(2, "test2", 0)
  }

  it should "be found by id" in {
    val resp = request(_ / "account" / "1")
    resp.getStatusCode should equal (200)
    val acc = json[JsObject](resp)
    checkAccount(acc)(1, "test1", 0)
  }

  it should "be credited and then transferred" in {
    val start = System.currentTimeMillis()

    val t1 = request(r => (r / "account" / "1" / "transaction" / "credit").POST << """{"value": 150.40}""")
    t1.getStatusCode should equal (200)
    val t2 = request(r => (r / "account" / "2" / "transaction" / "credit").POST << """{"value": 10.15}""")
    t2.getStatusCode should equal (200)

    val t3 = request(r => (r / "account" / "1" / "transaction" / "transfer").POST << """{"value": 100.05, "to": 2}""")
    t3.getStatusCode should equal (200)

    val end = System.currentTimeMillis()

    import scala.concurrent.duration._

    val list1 = request(_ / "account" / "1" / "transaction", 100 seconds)
    list1.getStatusCode should equal (200)

    val list2 = request(_ / "account" / "2" / "transaction", 100 seconds)
    list2.getStatusCode should equal (200)


    val transactions1 = json[JsArray](list1)
    transactions1.value.size should equal (2)
    checkTransaction(transactions1.value(0))(
      id = 1,
      start = start,
      end = end,
      amount = BigDecimal("150.40"),
      accountId = 1,
      comment = "Credited by the GOD!"
    )
    checkTransaction(transactions1.value(1))(
      id = 3,
      start = start,
      end = end,
      amount = BigDecimal("-100.05"),
      accountId = 1,
      comment = "Money transfer to account #2"
    )


    val transactions2 = json[JsArray](list2)
    transactions2.value.size should equal (2)
    checkTransaction(transactions2.value(0))(
      id = 2,
      start = start,
      end = end,
      amount = BigDecimal("10.15"),
      accountId = 2,
      comment = "Credited by the GOD!"
    )
    checkTransaction(transactions2.value(1))(
      id = 4,
      start = start,
      end = end,
      amount = BigDecimal("100.05"),
      accountId = 2,
      comment = "Money transfer from #1 ref #3"
    )
  }

  it should "have correct balance" in {
    val resp = request(_ / "account")
    val accounts = json[JsArray](resp)
    accounts.value.size should equal (2)

    checkAccount(accounts.value(0))(1, "test1", BigDecimal("50.35"))
    checkAccount(accounts.value(1))(2, "test2", BigDecimal("110.20"))
  }

  def json[T <: JsValue : Manifest](resp: Response): T = {
    val js = Json.parse(resp.getResponseBody)
    js shouldBe a [T]
    js.asInstanceOf[T]
  }

  //{"id":1,"profile":{"name":"test1"},"balance":0}
  def checkAccount(js: JsValue)(id: Int, name: String, balance: BigDecimal): JsObject = {
    val obj = js.asInstanceOf[JsObject]
    obj.value("id") should equal (JsNumber(id))
    obj.value("balance") should equal (JsNumber(balance))
    val profile = obj.value("profile").asInstanceOf[JsObject]
    profile.value("name") should equal (JsString(name))
    obj
  }

  def checkTransaction(js: JsValue)(id: Int, start: Long, end: Long, amount: BigDecimal, accountId: Int, comment: String): JsObject = {
    js shouldBe a [JsObject]
    val obj = js.asInstanceOf[JsObject]
    obj.value("id") should equal (JsNumber(id))
    obj.value("amount") should equal (JsNumber(amount))
    obj.value("time").asInstanceOf[JsNumber].value.toLong should (be >= start and be <= end)
    obj.value("accountId") should equal (JsNumber(accountId))
    obj.value("comment") should equal (JsString(comment))

    obj
  }
}
