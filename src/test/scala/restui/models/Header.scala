import io.github.iltotore.iron.refine
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, _}
import restui.models

class HeaderSpec extends AnyFunSpec with TableDrivenPropertyChecks {
  describe("Header") {
    it("new") {
      val table = Table(
        ("title", "value", "result"),
        ("if empty string", "", None),
        (
          "if header string",
          "content-type: application/json",
          models.Header(java.util.UUID.randomUUID.toString.refine[models.Id], "content-type", "application/json")
        )
      )

      forAll(table) { (title, s, result) =>
        withClue(s"$title:") { assert(models.Header(s).map(_.copy(id = result.get.id)) == result) }
      }
    }
  }
}
