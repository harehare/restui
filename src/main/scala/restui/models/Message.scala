package restui.models

enum Message:
  case Info(message: String)
  case Warn(message: String)
  case Error(message: String)
