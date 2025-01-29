module Arkham.Agenda.Cards.Intruders (intruders) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Treachery.Cards qualified as Treacheries

newtype Intruders = Intruders AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intruders :: AgendaCard Intruders
intruders = agenda (2, A) Intruders Cards.intruders (Static 9)

instance HasAbilities Intruders where
  getAbilities (Intruders a) = [mkAbility a 1 exploreAction_]

instance RunMessage Intruders where
  runMessage msg a@(Intruders attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = attrs.ability 1
      push $ Explore iid source (mapOneOf CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (investigatorDefeated attrs)
      getUnpoisoned
        >>= traverse_ \iid -> addCampaignCardToDeck iid DoNotShuffleIn Treacheries.poisoned
      pure a
    _ -> Intruders <$> liftRunMessage msg attrs
