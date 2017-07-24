package ipoemi.advancedscala.exercise.chapter08

import cats._, cats.data._, cats.implicits._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object `8.1` {
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  /*
  trait TestUptimeClient extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int]
  }
  */

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
  }
}

object `8.2` {

  import cats._, cats.data._, cats.implicits._
  import `8.1`._

  class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }
}

object Chapter08Main {
  def main(args: Array[String]): Unit = {
    import `8.1`._, `8.2`._

    def testTotalUptime() = {
      val hosts = Map("host1" -> 10, "host2" -> 6)
      val client = new TestUptimeClient(hosts)
      val service = new UptimeService(client)
      val actual = service.getTotalUptime(hosts.keys.toList)
      val expected = hosts.values.sum
      assert(actual == expected)
    }

    println(testTotalUptime())
  }
}
