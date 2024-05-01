package restui.models

enum Error:
  case IOError(cause: Throwable)
  case ConfigNotFound(path: String)
  case EnvNotFound(env: String)

  override def toString() =
    this match
      case IOError(cause)       => s"Error: ${cause.getMessage()}"
      case ConfigNotFound(path) => s"'${path}' could not be found."
      case EnvNotFound(env)     => s"Could not find $$${env} environment variable. Please set the environment variable $$${env}"
