package restui
import io.github.iltotore.iron.constraint.all._

package object models:
  type Id = (Not[Empty] & ValidUUID)
  type Env = Match["([-a-zA-Z0-9!@:%._\\+~#=/]*(\\$\\{.+\\})[-a-zA-Z0-9!@:%._\\+~#=/]*)"]
