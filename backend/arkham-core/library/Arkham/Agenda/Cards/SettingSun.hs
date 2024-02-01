module Arkham.Agenda.Cards.SettingSun (SettingSun (..), settingSun) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Prelude

newtype SettingSun = SettingSun AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

settingSun :: AgendaCard SettingSun
settingSun = agenda (2, A) SettingSun Cards.settingSun (Static 5)

instance HasAbilities SettingSun where
  getAbilities (SettingSun a) = [mkAbility a 1 exploreAction_]

instance RunMessage SettingSun where
  runMessage msg a@(SettingSun attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 1
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- selectList UneliminatedInvestigator
      pushAll $ map Resign iids
      pure a
    _ -> SettingSun <$> runMessage msg attrs
