module Arkham.EncounterCard where

import Arkham.Prelude

import Arkham.Act.Cards
import Arkham.Agenda.Cards
import Arkham.Asset.Cards
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Enemy.Cards
import Arkham.Event.Cards
import Arkham.Investigator.Cards (allEncounterInvestigatorCards)
import Arkham.Location.Cards
import Arkham.Story.Cards
import Arkham.Treachery.Cards

lookupEncounterCardDef :: CardCode -> CardDef
lookupEncounterCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allEncounterCards

allEncounterCards :: Map CardCode CardDef
allEncounterCards =
  allEncounterEnemyCards
    <> allLocationCards
    <> allSpecialLocationCards
    <> allEncounterTreacheryCards
    <> allEncounterAssetCards
    <> allEncounterEventCards
    <> allStoryCards
    <> allActCards
    <> allAgendaCards
    <> allEncounterInvestigatorCards -- technically player
