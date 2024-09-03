module Arkham.Agenda.Cards.Intruders (Intruders (..), intruders) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Treacheries

newtype Intruders = Intruders AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intruders :: AgendaCard Intruders
intruders = agenda (2, A) Intruders Cards.intruders (Static 9)

instance HasAbilities Intruders where
  getAbilities (Intruders a) = [mkAbility a 1 exploreAction_]

instance RunMessage Intruders where
  runMessage msg a@(Intruders attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 1
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- getInvestigatorIds
      unpoisoned <- getUnpoisoned
      pushAll
        $ [InvestigatorDefeated (toSource attrs) iid | iid <- iids]
        <> [ AddCampaignCardToDeck iid Treacheries.poisoned
           | iid <- unpoisoned
           ]
      pure a
    _ -> Intruders <$> runMessage msg attrs
