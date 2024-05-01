package restui.models

import restui.{models, views}

case class ResponseView(
    var body: views.StatefulTable[String],
    var header: views.StatefulTable[models.Header],
    var tab: views.StatefulTab[models.Response]
):
  def update(
      body: Option[views.StatefulTable[String]] = None,
      header: Option[views.StatefulTable[models.Header]] = None,
      tab: Option[views.StatefulTab[models.Response]] = None
  ) =
    this.body = body.getOrElse(this.body)
    this.header = header.getOrElse(this.header)
    this.tab = tab.getOrElse(this.tab)

  def current = tab.current
  def currentHeader = header.current
  def currentBody = body.current

  def nextTab =
    tab = tab.next
    this

  def nextBody =
    body = body.next
    this

  def nextHeader =
    header = header.next
    this

  def prevTab =
    tab = tab.previous
    this

  def prevBody =
    body = body.previous
    this

  def prevHeader =
    header = header.previous
    this
