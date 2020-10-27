package com.gjuhasz86.dupfinder.web.proto

import com.raquo.airstream.signal.Signal
import com.raquo.laminar.api.L._


object ImplicitHelpers {

  implicit class EventStreamOps[A](val self: EventStream[A]) extends AnyVal {
    def asText[B](f: A => B) =
      child.text <-- self.map(f(_).toString)
  }

  implicit class SignalOps[A](val self: Signal[A]) extends AnyVal {
    def asText[B](f: A => B) =
      child.text <-- self.map(f(_).toString)
  }

}
