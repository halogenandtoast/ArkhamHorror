module Arkham.Agenda.Cards.TheInfestationSpreads (
  TheInfestationSpreads (..),
  theInfestationSpreads,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.WakingNightmare.Helpers

newtype TheInfestationSpreads = TheInfestationSpreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theInfestationSpreads :: AgendaCard TheInfestationSpreads
theInfestationSpreads = agenda (2, A) TheInfestationSpreads Cards.theInfestationSpreads (Static 6)

instance HasAbilities TheInfestationSpreads where
  getAbilities (TheInfestationSpreads attrs) =
    [mkAbility attrs 1 $ ForcedAbility $ PhaseEnds #when #mythos]

instance RunMessage TheInfestationSpreads where
  runMessage msg a@(TheInfestationSpreads attrs) =
    case msg of
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        pushM makeInfestationTest
        pure a
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        corruptedOrderlies <- getSetAsideCardsMatching (cardIs Enemies.corruptedOrderly)
        suspiciousOrderlies <-
          zipWith ((,)) corruptedOrderlies <$> selectList (enemyIs Enemies.suspiciousOrderly)
        discarded <-
          scenarioFieldMap ScenarioDiscard (filter (`cardMatch` (cardIs Enemies.suspiciousOrderly)))
        inDeck <-
          scenarioFieldMap
            ScenarioEncounterDeck
            (filter (`cardMatch` (cardIs Enemies.suspiciousOrderly)) . unDeck)

        agentsOfAtlachNacha <- map toCard <$> gatherEncounterSet EncounterSet.AgentsOfAtlachNacha

        addTokens <- traverse addInfestationToken [#skull, #cultist]

        pushAll
          $ [ReplaceEnemy eid card Swap | (card, eid) <- suspiciousOrderlies]
          <> map RemoveFromEncounterDiscard discarded
          <> map RemoveFromEncounterDeck inDeck
          <> [ ShuffleEncounterDiscardBackIn
             , ShuffleCardsIntoDeck
                Deck.EncounterDeck
                (agentsOfAtlachNacha <> drop (length suspiciousOrderlies) corruptedOrderlies)
             ]
          <> addTokens
          <> [ advanceAgendaDeck attrs
             ]
        pure a
      _ -> TheInfestationSpreads <$> runMessage msg attrs
