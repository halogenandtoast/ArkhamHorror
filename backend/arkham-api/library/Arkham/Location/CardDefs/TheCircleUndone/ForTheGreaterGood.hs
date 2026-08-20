module Arkham.Location.CardDefs.TheCircleUndone.ForTheGreaterGood where

import Arkham.Location.CardDefs.Import

innerSanctum :: CardDef
innerSanctum =
  locationWithUnrevealed
    "05216"
    "Inner Sanctum"
    [Lodge, Sanctum]
    Star
    [Squiggle]
    "Inner Sanctum"
    [Lodge, Sanctum]
    Hourglass
    [Squiggle]
    ForTheGreaterGood

library :: CardDef
library =
  victory 1
    $ location
      "05212"
      "Library"
      [Lodge]
      Heart
      [Moon]
      ForTheGreaterGood

lobbyMembersOnly :: CardDef
lobbyMembersOnly =
  location
    "05207"
    ("Lobby" <:> "Members Only")
    [Lodge]
    Circle
    [T, Moon]
    ForTheGreaterGood

lobbyWeveBeenExpectingYou :: CardDef
lobbyWeveBeenExpectingYou =
  location
    "05206"
    ("Lobby" <:> "We've Been Expecting You")
    [Lodge]
    Circle
    [Diamond, T, Moon]
    ForTheGreaterGood

lodgeCatacombs :: CardDef
lodgeCatacombs =
  location
    "05213"
    "Lodge Catacombs"
    [Lodge, Sanctum]
    Squiggle
    [T, Star, Triangle, Square, Hourglass]
    ForTheGreaterGood

lodgeCellarMembersOnly :: CardDef
lodgeCellarMembersOnly =
  location
    "05209"
    ("Lodge Cellar" <:> "Members Only")
    [Lodge]
    T
    [Diamond, Circle, Squiggle]
    ForTheGreaterGood

lodgeCellarWeveBeenExpectingYou :: CardDef
lodgeCellarWeveBeenExpectingYou =
  location
    "05208"
    ("Lodge Cellar" <:> "We've Been Expecting You")
    [Lodge]
    T
    [Circle, Squiggle]
    ForTheGreaterGood

lodgeGatesMembersOnly :: CardDef
lodgeGatesMembersOnly =
  location
    "05205"
    ("Lodge Gates" <:> "Members Only")
    [Lodge]
    Diamond
    [T]
    ForTheGreaterGood

lodgeGatesWeveBeenExpectingYou :: CardDef
lodgeGatesWeveBeenExpectingYou =
  location
    "05204"
    ("Lodge Gates" <:> "We've Been Expecting You")
    [Lodge]
    Diamond
    [Circle]
    ForTheGreaterGood

lounge :: CardDef
lounge =
  location
    "05210"
    "Lounge"
    [Lodge]
    Moon
    [Circle, Heart, Plus]
    ForTheGreaterGood

returnToLounge :: CardDef
returnToLounge =
  locationWithUnrevealed
    "54043"
    "Lounge"
    [Lodge]
    Moon
    [Circle, Heart, Plus]
    "Lounge"
    [Lodge]
    Moon
    [Circle, Heart, Plus, Trefoil]
    ForTheGreaterGood

sanctumDoorwayCeremonyRoom :: CardDef
sanctumDoorwayCeremonyRoom =
  victory 2
    $ locationWithUnrevealed
      "05214"
      "Sanctum Doorway"
      [Lodge, Sanctum]
      Star
      [Squiggle]
      "Ceremony Room"
      [Lodge, Sanctum]
      Triangle
      [Squiggle]
      ForTheGreaterGood

sanctumDoorwayHoldingCells :: CardDef
sanctumDoorwayHoldingCells =
  locationWithUnrevealed
    "05215"
    "Sanctum Doorway"
    [Lodge, Sanctum]
    Star
    [Squiggle]
    "Holding Cells"
    [Lodge, Sanctum]
    Square
    [Squiggle]
    ForTheGreaterGood

vault :: CardDef
vault =
  victory 1
    $ location
      "05211"
      "Vault"
      [Lodge]
      Plus
      [Moon]
      ForTheGreaterGood
