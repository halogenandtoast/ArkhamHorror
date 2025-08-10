module Arkham.Agenda.Cards.SettingSun (settingSun) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher

newtype SettingSun = SettingSun AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

settingSun :: AgendaCard SettingSun
settingSun = agenda (2, A) SettingSun Cards.settingSun (Static 5)

instance HasAbilities SettingSun where
  getAbilities (SettingSun a) = [mkAbility a 1 exploreAction_]

instance RunMessage SettingSun where
  runMessage msg a@(SettingSun attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore iid (attrs.ability 1) (mapOneOf CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator resign
      pure a
    _ -> SettingSun <$> liftRunMessage msg attrs
