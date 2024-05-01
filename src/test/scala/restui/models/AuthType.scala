import io.github.iltotore.iron.refine
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, _}
import restui.models

class AuthTypeSpec extends AnyFunSpec with TableDrivenPropertyChecks {
  describe("AuthType") {
    it("toHeader") {
      val table = Table(
        ("title", "value", "result"),
        (
          "Basic auth",
          models.AuthType.Basic("user", "password"),
          models.Header(
            id = java.util.UUID.randomUUID.toString.refine[models.Id],
            name = "Authorization",
            value = "Basic dXNlcjpwYXNzd29yZA=="
          )
        ),
        (
          "Bearer auth",
          models.AuthType.BearerToken("token"),
          models.Header(
            id = java.util.UUID.randomUUID.toString.refine[models.Id],
            name = "Authorization",
            value = "Bearer token"
          )
        ),
        (
          "No auth",
          models.AuthType.NoAuth,
          None
        )
      )

      forAll(table) { (title, authType, result) =>
        withClue(s"$title:") { assert(authType.toHeader.map(_.copy(id = result.get.id)) == result) }
      }
    }
  }
}
