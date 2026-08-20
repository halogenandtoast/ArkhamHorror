module Arkham.Location.CardDefs.TheFeastOfHemlockVale.TheTwistedHollow where

import Arkham.Location.CardDefs.Import

bearDen :: CardDef
bearDen =
  locationWithUnrevealed_
    "10620"
    "Western Woods"
    [Forest, Dark]
    "Bear Den"
    [Forest, Lair, Dark]
    TheTwistedHollow

blightedGlade :: CardDef
blightedGlade =
  quantity 2
    $ locationWithUnrevealed_
      "10613"
      "Western Woods"
      [Forest, Dark]
      "Blighted Glade"
      [Forest, Blight, Dark]
      TheTwistedHollow

corpseGrove :: CardDef
corpseGrove =
  locationWithUnrevealed_
    "10619"
    "Western Woods"
    [Forest, Dark]
    "Corpse Grove"
    [Forest, Lair, Dark]
    TheTwistedHollow

crookedPath :: CardDef
crookedPath =
  quantity 3
    $ locationWithUnrevealed_
      "10618"
      "Western Woods"
      [Forest, Dark]
      "Crooked Path"
      [Forest, Dark]
      TheTwistedHollow

fecundThicket :: CardDef
fecundThicket =
  locationWithUnrevealed_
    "10615"
    "Western Woods"
    [Forest, Dark]
    "Fecund Thicket"
    [Forest, Dark]
    TheTwistedHollow

glimmeringWoods :: CardDef
glimmeringWoods =
  locationWithUnrevealed_
    "10612"
    "Western Woods"
    [Forest, Dark]
    "Glimmering Woods"
    [Forest, Dark]
    TheTwistedHollow

moonlitClearing :: CardDef
moonlitClearing =
  locationWithUnrevealed_
    "10617"
    "Western Woods"
    [Forest, Dark]
    "Moonlit Clearing"
    [Forest, Lair, Dark]
    TheTwistedHollow

mushroomGrove :: CardDef
mushroomGrove =
  quantity 2
    $ locationWithUnrevealed_
      "10616"
      "Western Woods"
      [Forest, Dark]
      "Mushroom Grove"
      [Forest, Dark]
      TheTwistedHollow

poisonedMarsh :: CardDef
poisonedMarsh =
  quantity 2
    $ locationWithUnrevealed_
      "10614"
      "Western Woods"
      [Forest, Dark]
      "Poisoned Marsh"
      [Forest, Blight, Dark]
      TheTwistedHollow

theTwistedHollow :: CardDef
theTwistedHollow =
  victory 1
    $ locationWithUnrevealed_
      "10621"
      "Western Woods"
      [Forest, Dark]
      "The Twisted Hollow"
      [Forest, Dark]
      TheTwistedHollow
