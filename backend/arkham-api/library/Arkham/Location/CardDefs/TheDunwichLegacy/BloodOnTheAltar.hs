{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDunwichLegacy.BloodOnTheAltar where

import Arkham.Location.CardDefs.Import

bishopsBrook_202 :: CardDef
bishopsBrook_202 =
  location
    "02202"
    "Bishop's Brook"
    [Dunwich]
    Square
    [Plus, Circle, Triangle]
    BloodOnTheAltar

bishopsBrook_203 :: CardDef
bishopsBrook_203 =
  location
    "02203"
    "Bishop's Brook"
    [Dunwich]
    Square
    [Plus, Circle, Triangle]
    BloodOnTheAltar

burnedRuins_204 :: CardDef
burnedRuins_204 =
  location
    "02204"
    "Burned Ruins"
    [Dunwich]
    Triangle
    [Square, Diamond]
    BloodOnTheAltar

burnedRuins_205 :: CardDef
burnedRuins_205 =
  location
    "02205"
    "Burned Ruins"
    [Dunwich]
    Triangle
    [Square, Diamond]
    BloodOnTheAltar

congregationalChurch_208 :: CardDef
congregationalChurch_208 =
  location
    "02208"
    "Congregational Church"
    [Dunwich]
    Diamond
    [Plus, Triangle, Squiggle]
    BloodOnTheAltar

congregationalChurch_209 :: CardDef
congregationalChurch_209 =
  location
    "02209"
    "Congregational Church"
    [Dunwich]
    Diamond
    [Plus, Triangle, Squiggle]
    BloodOnTheAltar

houseInTheReeds_210 :: CardDef
houseInTheReeds_210 =
  location
    "02210"
    "House in the Reeds"
    [Dunwich]
    Squiggle
    [Diamond, Moon]
    BloodOnTheAltar

houseInTheReeds_211 :: CardDef
houseInTheReeds_211 =
  location
    "02211"
    "House in the Reeds"
    [Dunwich]
    Squiggle
    [Diamond, Moon]
    BloodOnTheAltar

osbornsGeneralStore_206 :: CardDef
osbornsGeneralStore_206 =
  location
    "02206"
    "Osborn's General Store"
    [Dunwich]
    Circle
    [Moon, Square]
    BloodOnTheAltar

osbornsGeneralStore_207 :: CardDef
osbornsGeneralStore_207 =
  location
    "02207"
    "Osborn's General Store"
    [Dunwich]
    Circle
    [Moon, Square]
    BloodOnTheAltar

schoolhouse_212 :: CardDef
schoolhouse_212 =
  location
    "02212"
    "Schoolhouse"
    [Dunwich]
    Moon
    [Plus, Squiggle, Circle]
    BloodOnTheAltar

schoolhouse_213 :: CardDef
schoolhouse_213 =
  location
    "02213"
    "Schoolhouse"
    [Dunwich]
    Moon
    [Plus, Squiggle, Circle]
    BloodOnTheAltar

theHiddenChamber :: CardDef
theHiddenChamber =
  victory 2
    $ singleSided
    $ location
      "02214"
      ("The Hidden Chamber" <:> "Prison of the Beast")
      [Dunwich]
      NoSymbol
      []
      BloodOnTheAltar

villageCommons :: CardDef
villageCommons =
  location
    "02201"
    "Village Commons"
    [Dunwich, Central]
    Plus
    [Square, Circle, Moon]
    BloodOnTheAltar
