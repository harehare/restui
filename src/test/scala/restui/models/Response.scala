import scala.concurrent.duration.{Duration, MILLISECONDS, SECONDS}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, _}
import restui.models

class ResponseSpec extends AnyFunSpec with TableDrivenPropertyChecks {
  describe("Response") {
    it("toDurationString") {
      val table = Table(
        ("title", "value", "result"),
        (
          "100ms",
          models.ResponseBody(
            models.ResponseStatus(200),
            "",
            models.ResponseHeaders(models.Headers(Array())),
            Duration(100, MILLISECONDS)
          ),
          "100ms",
        ),
        (
          "1.2s",
          models.ResponseBody(
            models.ResponseStatus(200),
            "",
            models.ResponseHeaders(models.Headers(Array())),
            Duration(1200, MILLISECONDS)
          ),
          "1.2s",
        ),
        (
          "80s",
          models.ResponseBody(
            models.ResponseStatus(200),
            "",
            models.ResponseHeaders(models.Headers(Array())),
            Duration(80, SECONDS)
          ),
          "80s",
        )
      )
      forAll(table) { (title, response, result) =>
        withClue(s"$title:") { assert(response.toDurationString == result) }
      }
    }
  }
}
