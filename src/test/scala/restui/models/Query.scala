import io.github.iltotore.iron.refine
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, _}
import restui.models

class QuerySpec extends AnyFunSpec with TableDrivenPropertyChecks {
  describe("Query") {
    it("new") {
      val table = Table(
        ("title", "value", "result"),
        ("if empty string", "", None),
        (
          "if query string",
          "name=value",
          models.Query(java.util.UUID.randomUUID.toString.refine[models.Id], "name", "value")
        ),
        (
          "if query string in Percent-encoding",
          "name=%3Fvalue%2F",
          models.Query(java.util.UUID.randomUUID.toString.refine[models.Id], "name", "?value/")
        )
      )

      forAll(table) { (title, s, result) =>
        withClue(s"$title:") { assert(models.Query(s).map(_.copy(id = result.get.id)) == result) }
      }
    }
  }
}
