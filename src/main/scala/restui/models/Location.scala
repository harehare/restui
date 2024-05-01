package restui.models

// TODO: Param の各 table, Response も追加する
enum Location:
  case Collection
  case Request
  case Method
  case RequestUrl
  case Param
  case Response

  def prev(
      hasCollection: Boolean,
      hasRequest: Boolean,
      hasResponse: Boolean
  ): Location = this match
    case Collection if hasResponse => Response
    case Collection                => Param
    case Param                     => RequestUrl
    case Method                    => Request
    case RequestUrl                => Method
    case Request                   => Collection
    case Response                  => Param

  def next(
      hasCollection: Boolean,
      hasRequest: Boolean,
      hasResponse: Boolean
  ): Location = this match
    case Collection if !hasRequest => Collection
    case Collection                => Request
    case Request                   => Method
    case Method                    => RequestUrl
    case RequestUrl                => Param
    case Param if !hasResponse     => Collection
    case Param                     => Response
    case Response                  => Collection

  def isRequestUrl = this == RequestUrl
  def isMethod = this == Method
  def isParam = this == Param
  def isCollection = this == Collection
