module Arkham.Location.CardDefs.TheFeastOfHemlockVale.DayOfTheFeast where

import Arkham.Location.CardDefs.Import

theCrossroadsEvening :: CardDef
theCrossroadsEvening =
  otherSideIs "10690a"
    $ location
      "10690b"
      "The Crossroads"
      [HemlockVale, Central]
      Diamond
      [Triangle, Square, Star, Circle, Heart, Moon]
      DayOfTheFeast

theCrossroadsMorning :: CardDef
theCrossroadsMorning =
  otherSideIs "10690b"
    $ location
      "10690a"
      "The Crossroads"
      [HemlockVale, Central]
      Diamond
      [Triangle, Square, Star, Circle, Heart, Moon]
      DayOfTheFeast

theOldMillEvening :: CardDef
theOldMillEvening =
  otherSideIs "10691a"
    $ location "10691b" "The Old Mill" [HemlockVale] Heart [Diamond, Moon] DayOfTheFeast

theOldMillMorning :: CardDef
theOldMillMorning =
  otherSideIs "10691b"
    $ location "10691a" "The Old Mill" [HemlockVale] Heart [Diamond, Moon] DayOfTheFeast
