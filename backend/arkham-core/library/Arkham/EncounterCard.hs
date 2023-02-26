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
  HashMap.map (SomeCardDef SEnemyType) allEncounterEnemyCards
    <> HashMap.map (SomeCardDef SLocationType) allLocationCards
    <> HashMap.map (SomeCardDef SLocationType) allSpecialLocationCards
    <> HashMap.map (SomeCardDef STreacheryType) allEncounterTreacheryCards
    <> HashMap.map (SomeCardDef SEncounterAssetType) allEncounterAssetCards
    <> HashMap.map (SomeCardDef SStoryType) allStoryCards
    <> HashMap.map (SomeCardDef SActType) allActCards
    <> HashMap.map (SomeCardDef SAgendaType) allAgendaCards
