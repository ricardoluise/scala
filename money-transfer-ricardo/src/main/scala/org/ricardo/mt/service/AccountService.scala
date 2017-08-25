package org.ricardo.mt.service

import org.ricardo.mt.util.Money

import scala.concurrent.Future


trait AccountService {
  def addAccount(profile: Profile): Future[Account]
  def getAccounts(): Future[Seq[Account]]
  def getAccount(id: Int): Future[Option[Account]]
}


case class Profile(name: String)
case class Account(id: Int, profile: Profile, balance: Money)
