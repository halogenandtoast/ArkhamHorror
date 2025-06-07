module Arkham.Act.Cards.IntoTheRuins (intoTheRuins) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype IntoTheRuins = IntoTheRuins ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheRuins :: ActCard IntoTheRuins
intoTheRuins = act (1, A) IntoTheRuins Cards.intoTheRuins (groupClueCost $ PerPlayer 3)

instance HasAbilities IntoTheRuins where
  getAbilities (IntoTheRuins a) = withBaseAbilities a [mkAbility a 1 exploreAction_]

instance RunMessage IntoTheRuins where
  runMessage msg a@(IntoTheRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = attrs.ability 1
      push $ Explore iid source $ mapOneOf CardWithPrintedLocationSymbol locationSymbols
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      chamberOfTime <- fetchCard Locations.chamberOfTime
      shuffleCardsIntoDeck ExplorationDeck (only chamberOfTime)
      hasChalk <- getAnyHasSupply Chalk
      unless hasChalk $ addToVictory attrs
      advanceActDeck attrs
      pure a
    _ -> IntoTheRuins <$> liftRunMessage msg attrs
