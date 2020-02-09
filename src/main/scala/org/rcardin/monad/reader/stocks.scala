package org.rcardin.monad.reader

import org.rcardin.monad.reader.ReaderMonad.Reader

object ReaderMonad {

  case class Reader[From, To](f: From => To) {
    def apply(input: From): To =
      f(input)

    def map[NewTo](transformation: To => NewTo): Reader[From, NewTo] =
      Reader(c => transformation(f(c)))

    def flatMap[NewTo](transformation: To => Reader[From, NewTo]): Reader[From, NewTo] =
      Reader(c => transformation(f(c))(c))
  }

  def pure[From, To](a: To): Reader[From, To] = Reader((c: From) => a)
}

object stocks {

  trait StockRepository {
    def findAll(): Map[String, Double]
    def sell(stock: String, quantity: Double): Double
    def buy(stock: String, amount: Double): Double
  }

  object Stocks {
    def findAll(): Reader[StockRepository, Map[String, Double]] = Reader {
      repo => repo.findAll()
    }
    def sell(stock: String, quantity: Double): Reader[StockRepository, Double] = Reader {
      repo => repo.sell(stock, quantity)
    }
    def buy(stock: String, amount: Double): Reader[StockRepository, Double] = Reader {
      repo => repo.buy(stock, amount)
    }
  }

  def investInStockWithMinValue: Reader[StockRepository, Double] = {
    Stocks.findAll()
      .map(stocks => stocks.minBy(_._2))
      .map { case (stock, _) => stock }
      .flatMap(stock => Stocks.buy(stock, 1000.0D))
  }

  def investInStockWithMinValueUsingForComprehension: Reader[StockRepository, Unit] =
    for {
      stocks <- Stocks.findAll()
      minStock <- ReaderMonad.pure(stocks.minBy(_._2)._1)
      _ <- Stocks.buy(minStock, 1000.0D)
    } yield ()

  def main(args: Array[String]): Unit = {
    val stockRepo = new StockRepository {
      override def findAll(): Map[String, Double] = Map("AMZN" -> 1631.17, "GOOG" -> 1036.05, "TSLA" -> 346.00)
      override def sell(stock: String, quantity: Double): Double = quantity * findAll()(stock)
      override def buy(stock: String, amount: Double): Double = findAll()(stock) / amount
    }

    investInStockWithMinValueUsingForComprehension.apply(stockRepo)
  }
}
