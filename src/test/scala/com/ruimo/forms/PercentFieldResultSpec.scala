package com.ruimo.forms

import com.ruimo.scoins.Percent
import org.specs2.mutable._

class PercentFieldResultSpec extends Specification {
  "Percent Field" should {
    "Can parse string" in {
      PercentFieldResult.parseString("") === Left(PercentFieldResult.Empty)
      PercentFieldResult.parseString("a") === Left(PercentFieldResult.NotNumber)
      PercentFieldResult.parseString(" ") === Left(PercentFieldResult.Empty)
      PercentFieldResult.parseString("0") === Right(Percent(0))
      PercentFieldResult.parseString(" 0") === Right(Percent(0))
      PercentFieldResult.parseString("0 ") === Right(Percent(0))
      PercentFieldResult.parseString("0 1") === Left(PercentFieldResult.NotNumber)
      PercentFieldResult.parseString("-1") === Right(Percent(-1))
      PercentFieldResult.parseString("120") === Right(Percent(120))
      PercentFieldResult.parseString("120", max = Some(Percent(119))) === Left(PercentFieldResult.TooBig(Percent(119)))
      PercentFieldResult.parseString("120", max = Some(Percent(120))) === Right(Percent(120))
      PercentFieldResult.parseString("120", max = Some(Percent(121))) === Right(Percent(120))
      PercentFieldResult.parseString("20", min = Some(Percent(19))) === Right(Percent(20))
      PercentFieldResult.parseString("20", min = Some(Percent(20))) === Right(Percent(20))
      PercentFieldResult.parseString("20", min = Some(Percent(21))) === Left(PercentFieldResult.TooSmall(Percent(21)))
    }
  }
}
