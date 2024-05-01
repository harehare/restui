import io.github.iltotore.iron.refine
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, _}
import restui.models

class RequestSpec extends AnyFunSpec with TableDrivenPropertyChecks {
  describe("Request") {
    it("toCurlCommand") {
      val table = Table(
        ("title", "value", "env", "result"),
        (
          "default value",
          models.Request.create(),
          None,
          "curl -X GET http://localhost:3000",
        ),
        (
          "has headers",
          models.Request
            .create()
            .copy(headers = models.Headers(Array(models.Header(java.util.UUID.randomUUID.toString, "name", "value").get))),
          None,
          "curl -X GET -H 'name=value' http://localhost:3000",
        ),
        (
          "has headers and queries",
          models.Request
            .create()
            .copy(
              queryParams = models.QueryParams(Array(models.Query(java.util.UUID.randomUUID.toString, "query", "1").get)),
              headers = models.Headers(Array(models.Header(java.util.UUID.randomUUID.toString, "name", "value").get))
            ),
          None,
          "curl -X GET -H 'name=value' --data-urlencode 'query=1' http://localhost:3000",
        ),
        (
          "has body and headers and queries",
          models.Request
            .create()
            .copy(
              method = models.Method.Post,
              queryParams = models.QueryParams(Array(models.Query(java.util.UUID.randomUUID.toString, "query", "1").get)),
              headers = models.Headers(Array(models.Header(java.util.UUID.randomUUID.toString, "name", "value").get)),
              body = models.Body(models.BodyType.Raw("raw"))
            ),
          None,
          "curl -X POST -H 'name=value' --data-urlencode 'query=1' -d 'raw' http://localhost:3000",
        ),
        (
          "has env and body and headers and queries",
          models.Request
            .create()
            .copy(
              url = "${HOST}/test".refine,
              method = models.Method.Post,
              queryParams =
                models.QueryParams(Array(models.Query(java.util.UUID.randomUUID.toString, "query", "${QUERY}").get)),
              headers = models.Headers(
                Array(
                  models.Header(java.util.UUID.randomUUID.toString, "name", "value").get,
                  models.Header(java.util.UUID.randomUUID.toString, "content-type", "${CONTENT-TYPE-JSON}").get
                )
              ),
              body = models.Body(models.BodyType.Raw("raw"))
            ),
          Some(
            restui.Env(Map("HOST" -> "http://localhost:8080", "CONTENT-TYPE-JSON" -> "application/json", "QUERY" -> "query"))
          ),
          "curl -X POST -H 'name=value' -H 'content-type=application/json' --data-urlencode 'query=query' -d 'raw' http://localhost:8080/test",
        )
      )
      forAll(table) { (title, request, env, result) =>
        withClue(s"$title:") { assert(request.toCurlCommand(env) == result) }
      }
    }
  }
}
