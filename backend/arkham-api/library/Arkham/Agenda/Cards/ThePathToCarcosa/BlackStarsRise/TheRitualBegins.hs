module Arkham.Agenda.Cards.ThePathToCarcosa.BlackStarsRise.TheRitualBegins (theRitualBegins) where

import Arkham.Ability
import Arkham.Agenda.CardDefs.ThePathToCarcosa.BlackStarsRise qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Projection

newtype TheRitualBegins = TheRitualBegins AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBegins :: AgendaCard TheRitualBegins
theRitualBegins =
  agenda (1, C) TheRitualBegins Cards.theRitualBegins (Static 5)

instance HasAbilities TheRitualBegins where
  getAbilities (TheRitualBegins a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage TheRitualBegins where
  runMessage msg a@(TheRitualBegins attrs) = runQueueT $ case msg of
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
      eachInvestigator \iid -> drawCards iid (attrs.ability 1) 1
      pure a
    _ -> TheRitualBegins <$> liftRunMessage msg attrs
