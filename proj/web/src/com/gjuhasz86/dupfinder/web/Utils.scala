package com.gjuhasz86.dupfinder.web

import slinky.core.SyntheticEvent

object Utils {

  def stopBubble[E <: SyntheticEvent[_, _], R](e: E)(f: E => R): E => R = {
    e.stopPropagation()
    f
  }

}
