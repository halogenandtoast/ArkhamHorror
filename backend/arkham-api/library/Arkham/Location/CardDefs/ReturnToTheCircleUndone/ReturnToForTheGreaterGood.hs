module Arkham.Location.CardDefs.ReturnToTheCircleUndone.ReturnToForTheGreaterGood where

import Arkham.Location.CardDefs.Import

relicStorage :: CardDef
relicStorage =
  locationWithUnrevealed
    "54044"
    "Hidden Passageway"
    [Lodge]
    Trefoil
    [Moon]
    "Relic Storage"
    [Lodge]
    Trefoil
    [Moon]
    ReturnToForTheGreaterGood

shroudedArchive :: CardDef
shroudedArchive =
  locationWithUnrevealed
    "54045"
    "Sanctum Doorway"
    [Lodge, Sanctum]
    Star
    [Squiggle]
    "Shrouded Archive"
    [Lodge, Sanctum]
    Triangle
    [Squiggle]
    ReturnToForTheGreaterGood
