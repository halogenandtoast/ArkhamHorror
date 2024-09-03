module Arkham.Act.Cards.IntoTheRuins (IntoTheRuins (..), intoTheRuins) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Deck

newtype IntoTheRuins = IntoTheRuins ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheRuins :: ActCard IntoTheRuins
intoTheRuins = act (1, A) IntoTheRuins Cards.intoTheRuins (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance HasAbilities IntoTheRuins where
  getAbilities (IntoTheRuins a) = withBaseAbilities a [mkAbility a 1 exploreAction_]

instance RunMessage IntoTheRuins where
  runMessage msg a@(IntoTheRuins attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 1
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      chamberOfTime <- getSetAsideCard Locations.chamberOfTime
      hasChalk <- getAnyHasSupply Chalk
      pushAll
        $ [ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey ExplorationDeck) [chamberOfTime]]
        <> [AddToVictory (toTarget attrs) | not hasChalk]
        <> [advanceActDeck attrs]
      pure a
    _ -> IntoTheRuins <$> runMessage msg attrs
