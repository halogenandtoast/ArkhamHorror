module Arkham.Agenda.Cards.TheRitualBeginsBlackStarsRise (
  TheRitualBeginsBlackStarsRise (..),
  theRitualBeginsBlackStarsRise,
) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Import.Lifted
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Projection

newtype TheRitualBeginsBlackStarsRise = TheRitualBeginsBlackStarsRise AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBeginsBlackStarsRise :: AgendaCard TheRitualBeginsBlackStarsRise
theRitualBeginsBlackStarsRise =
  agenda (1, C) TheRitualBeginsBlackStarsRise Cards.theRitualBeginsBlackStarsRise (Static 5)

instance HasAbilities TheRitualBeginsBlackStarsRise where
  getAbilities (TheRitualBeginsBlackStarsRise a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage TheRitualBeginsBlackStarsRise where
  runMessage msg a@(TheRitualBeginsBlackStarsRise attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide D attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      shuffleCardsIntoDeck Deck.EncounterDeck =<< getSetAsideCardsMatching (CardWithTitle "Rift Seeker")
      mAgenda1A <- selectOne $ AgendaWithSequence $ AS.Sequence 1 A
      for_ mAgenda1A \a1aId -> do
        a1aDoom <- field AgendaDoom a1aId
        if a1aDoom > 3 then markDoubt else markConviction
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      push AdvanceAgendaIfThresholdSatisfied
      eachInvestigator \iid -> drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> TheRitualBeginsBlackStarsRise <$> liftRunMessage msg attrs
