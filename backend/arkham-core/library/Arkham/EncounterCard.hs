module Arkham.EncounterCard where

import Arkham.Prelude

import Arkham.Asset.Cards
import Arkham.Enemy.Cards
import Arkham.Location.Cards
import Arkham.Treachery.Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef

lookupEncounterCardDef :: CardCode -> CardDef
lookupEncounterCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allEncounterCards

allEncounterCards :: Map CardCode CardDef
allEncounterCards =
  allEncounterEnemyCards
    <> allLocationCards
    <> allEncounterTreacheryCards
    <> allEncounterAssetCards
