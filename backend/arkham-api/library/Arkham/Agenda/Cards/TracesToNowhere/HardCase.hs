module Arkham.Agenda.Cards.TracesToNowhere.HardCase (hardCase) where

import Arkham.Agenda.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype HardCase = HardCase AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardCase :: AgendaCard HardCase
hardCase = agenda (1, A) HardCase Cards.hardCase (Static 4)

instance HasAbilities HardCase where
  getAbilities (HardCase a) = [abductInsteadOfDefeat a 1]

instance RunMessage HardCase where
  runMessage msg a@(HardCase attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      abductInsteadOfDefeatEffect ws
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      cards <-
        getSetAsideCardsMatching
          $ CardFromEncounterSet Set.DimensionalMadness
          <> oneOf [#enemy, #treachery]
          <> not_ (cardIs Enemies.abductorThatWhichLurksInShadows)
      shuffleCardsIntoDeck Deck.EncounterDeck cards
      advanceAgendaDeck attrs
      pure a
    _ -> HardCase <$> liftRunMessage msg attrs
