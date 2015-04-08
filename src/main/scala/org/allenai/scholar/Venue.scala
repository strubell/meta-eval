package org.allenai.scholar

import spray.json.DefaultJsonProtocol._

case class Venue(name: String) {
  def nonEmpty = name match {
    case "" => this
    case _ => Venue("nonEmpty")
  }

  def ifDefined = name match {
    case "" => None
    case _ => Some(this)
  }
  override def toString = name
}

object Venue {
  implicit val JsFormat = jsonFormat1(Venue.apply)
}
