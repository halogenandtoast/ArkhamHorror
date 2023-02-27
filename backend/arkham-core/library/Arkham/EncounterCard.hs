module Arkham.EncounterCard where

import Arkham.Prelude

import Arkham.Act.Cards
import Arkham.Agenda.Cards
import Arkham.Asset.Cards
import Arkham.Enemy.Cards
import Arkham.Location.Cards
import Arkham.Story.Cards
import Arkham.Treachery.Cards
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Data.HashMap.Strict qualified as HashMap

lookupEncounterCardDef :: CardCode -> SomeCardDef
lookupEncounterCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allEncounterCards

allEncounterCards :: HashMap CardCode SomeCardDef
allEncounterCards =
  HashMap.map toCardDef allEncounterEnemyCards
    <> HashMap.map toCardDef allLocationCards
    <> HashMap.map toCardDef allSpecialLocationCards
    <> HashMap.map toCardDef allEncounterTreacheryCards
    <> HashMap.map toCardDef allEncounterAssetCards
    <> HashMap.map toCardDef allStoryCards
    <> HashMap.map toCardDef allActCards
    <> HashMap.map toCardDef allAgendaCards
