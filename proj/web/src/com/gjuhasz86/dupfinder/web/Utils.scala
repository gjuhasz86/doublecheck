package com.gjuhasz86.dupfinder.web

import com.gjuhasz86.dupfinder.shared.StatKey
import io.circe.KeyDecoder
import io.circe.KeyEncoder

object Utils {
  implicit def skEnc[T]: KeyEncoder[StatKey[T]] = _.name
  implicit val skDec: KeyDecoder[StatKey[_]] = StatKey.of(_)
}
