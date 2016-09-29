package com.interviews.zenreach

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestMaxMatch extends Specification {
  import MaxMatch._

  "MaxMatch" should {

    "parseName should compute correctly" in {
      parseName("Jack Abraham") === (4,7,0)
      parseName("John Evans")   === (3,6,0)
      parseName("Ted Dziuba")   === (4,5,0)
      parseName("iPad 2 - 4-pack")        === (3,5,0)
      parseName("Girl Scouts Thin Mints") === (5,14,0)
      parseName("Nerf Crossbow")          === (3,9,0)
    }

    "maxCommonDivisor should compute correctly" in {
      maxCommonDivisor(12,9) === 3
      maxCommonDivisor(9,12) === 3
      maxCommonDivisor(9,1) === 1
      maxCommonDivisor(1,9) === 1
      maxCommonDivisor(9,0) === 9
      maxCommonDivisor(0,9) === 9
      maxCommonDivisor(0,0) === 0
    }

    "computeMaxMatchStr should calculate correctly for given test cases" in {
      val test1 = "Jack Abraham,John Evans,Ted Dziuba;iPad 2 - 4-pack,Girl Scouts Thin Mints,Nerf Crossbow"
      val test2 = "Jeffery Lebowski,Walter Sobchak,Theodore Donald Kerabatsos,Peter Gibbons,Michael Bolton,Samir Nagheenanajar;Half & Half,Colt M1911A1,16lb bowling ball,Red Swingline Stapler,Printer paper,Vibe Magazine Subscriptions - 40 pack"
      val test3 = "Jareau Wade,Rob Eroh,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson;Batman No. 1,Football - Official Size,Bass Amplifying Headphones,Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,Dom Perignon 2000 Vintage"
      val test4 = "Jareau Wade,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson;Batman No. 1,Football - Official Size,Bass Amplifying Headphones,Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,Dom Perignon 2000 Vintage"

      computeMaxMatchStr(test1) === "21.00"
      computeMaxMatchStr(test2) === "83.50"
      computeMaxMatchStr(test3) === "71.25"
      computeMaxMatchStr(test4) === "71.25"
    }
  }
}
