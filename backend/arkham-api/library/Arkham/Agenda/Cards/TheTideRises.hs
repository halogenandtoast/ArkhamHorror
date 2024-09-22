module Arkham.Agenda.Cards.TheTideRises (TheTideRises (..), theTideRises) where

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

newtype TheTideRises = TheTideRises AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTideRises :: AgendaCard TheTideRises
theTideRises = agenda (1, A) TheTideRises Cards.theTideRises (Static 5)

instance HasAbilities TheTideRises where
  getAbilities (TheTideRises a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage TheTideRises where
  runMessage msg a@(TheTideRises attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      shuffleCardsIntoDeck Deck.EncounterDeck =<< getSetAsideCardsMatching (CardWithTitle "Tidal Terror")
      mAgenda1C <- selectOne $ AgendaWithSequence $ AS.Sequence 1 C
      for_ mAgenda1C \a1cId -> do
        a1cDoom <- field AgendaDoom a1cId
        if a1cDoom > 3 then markDoubt else markConviction
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      push AdvanceAgendaIfThresholdSatisfied
      eachInvestigator \iid -> gainResourcesIfCan iid (attrs.ability 1) 2
      pure a
    _ -> TheTideRises <$> liftRunMessage msg attrs
