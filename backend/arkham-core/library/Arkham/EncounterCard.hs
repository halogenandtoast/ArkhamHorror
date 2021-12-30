module Arkham.EncounterCard where

import Arkham.Prelude

import Arkham.Asset.Cards
import Arkham.Enemy.Cards
import Arkham.Location.Cards
import Arkham.Story.Cards
import Arkham.Treachery.Cards
import Arkham.Card.CardCode
import Arkham.Card.CardDef

lookupEncounterCardDef :: CardCode -> CardDef
lookupEncounterCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allEncounterCards

allEncounterCards :: HashMap CardCode CardDef
allEncounterCards =
  allEncounterEnemyCards
    <> allLocationCards
    <> allEncounterTreacheryCards
    <> allEncounterAssetCards
    <> allStoryCards
