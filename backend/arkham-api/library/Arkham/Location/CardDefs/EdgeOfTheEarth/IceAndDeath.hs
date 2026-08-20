module Arkham.Location.CardDefs.EdgeOfTheEarth.IceAndDeath where

import Arkham.Location.CardDefs.Import

barrierCamp :: CardDef
barrierCamp =
  withMeta ("shelter", Number 7)
    $ location
      "08512"
      "Barrier Camp"
      [Glacier, Uncharted]
      Moon
      [Droplet, Equals]
      IceAndDeath

broadSnowdrifts :: CardDef
broadSnowdrifts =
  withMeta ("shelter", Number 4)
    $ location
      "08506"
      "Broad Snowdrifts"
      [Mainland, Uncharted]
      Square
      [Diamond, Hourglass, Star, Squiggle]
      IceAndDeath

crashSite :: CardDef
crashSite =
  withMeta ("shelter", Number 0)
    $ location "08502" "Crash Site" mempty Circle [Diamond, Triangle, Heart] IceAndDeath

crystallineCavern :: CardDef
crystallineCavern =
  withMeta ("shelter", Number 8)
    $ location
      "08514"
      "Crystalline Cavern"
      [Mountains, Uncharted]
      Hourglass
      [Equals, Square]
      IceAndDeath

frigidCave :: CardDef
frigidCave =
  withMeta ("shelter", Number 6)
    $ location
      "08511"
      "Frigid Cave"
      [Mountains, Uncharted]
      Plus
      [Equals]
      IceAndDeath

frozenShores :: CardDef
frozenShores =
  withMeta ("shelter", Number 2)
    $ location "08503" "Frozen Shores" [Mainland] Diamond [Circle, Triangle, Heart, Square] IceAndDeath

icebreakerLanding :: CardDef
icebreakerLanding =
  withMeta ("shelter", Number 5)
    $ location
      "08510"
      "Icebreaker Landing"
      [Glacier, Uncharted]
      Trefoil
      [Droplet]
      IceAndDeath

icyWastes :: CardDef
icyWastes =
  withMeta ("shelter", Number 4)
    $ location
      "08507"
      "Broad Snowdrifts"
      [Glacier, Uncharted]
      Droplet
      [Heart, Star, Moon, Trefoil]
      IceAndDeath

precariousIceSheet :: CardDef
precariousIceSheet =
  withMeta ("shelter", Number 2)
    $ location
      "08505"
      "Precarious Ice Sheet"
      [Glacier]
      Heart
      [Circle, Diamond, Triangle, Droplet]
      IceAndDeath

remnantsOfLakesCamp :: CardDef
remnantsOfLakesCamp =
  withMeta ("shelter", Number 7)
    $ location
      "08513"
      "Remnants of Lake's Camp"
      [Mainland, Uncharted]
      Star
      [Square, Droplet]
      IceAndDeath

rockyCrags :: CardDef
rockyCrags =
  withMeta ("shelter", Number 3)
    $ location
      "08508"
      "Rocky Crags"
      [Mountains, Uncharted]
      Equals
      [Triangle, Hourglass, Moon, Plus]
      IceAndDeath

snowGraves :: CardDef
snowGraves =
  withMeta ("shelter", Number 5)
    $ location
      "08509"
      "Snow Graves"
      [Mainland, Uncharted]
      Squiggle
      [Square]
      IceAndDeath

treacherousPath :: CardDef
treacherousPath =
  withMeta ("shelter", Number 1)
    $ location
      "08504"
      "Treacherous Path"
      [Mountains]
      Triangle
      [Circle, Diamond, Heart, Equals]
      IceAndDeath
