module Arkham.Agenda.Cards.TheHangedManXII (
  TheHangedManXII (..),
  theHangedManXII,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Trait (Trait (Spectral))
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheHangedManXII = TheHangedManXII AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHangedManXII :: AgendaCard TheHangedManXII
theHangedManXII = agenda (1, A) TheHangedManXII Cards.theHangedManXII (Static 8)

instance RunMessage TheHangedManXII where
  runMessage msg a@(TheHangedManXII attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        flippableLocations <-
          selectList $ LocationWithoutModifier CannotBeFlipped <> NotLocation (LocationWithTrait Spectral)
        lead <- getLead
        spectralWatcher <- getSetAsideCard Enemies.theSpectralWatcher
        hangmansBrook <- getJustLocationByName "Hangman's Brook"
        createSpectralWatcher <-
          createEnemyAt_
            spectralWatcher
            hangmansBrook
            Nothing
        watchersGrasps <- getSetAsideCardsMatching $ cardIs Treacheries.watchersGrasp
        spectralDiscards <- getSpectralDiscards

        pushAll
          $ [Flip lead (toSource attrs) (toTarget location) | location <- flippableLocations]
          <> [ createSpectralWatcher
             , ShuffleCardsIntoDeck
                (EncounterDeckByKey SpectralEncounterDeck)
                (watchersGrasps <> map EncounterCard spectralDiscards)
             , advanceAgendaDeck attrs
             ]
        pure a
      _ -> TheHangedManXII <$> runMessage msg attrs
