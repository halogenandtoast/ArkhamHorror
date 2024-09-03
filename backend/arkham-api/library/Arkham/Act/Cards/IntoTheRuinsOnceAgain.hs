module Arkham.Act.Cards.IntoTheRuinsOnceAgain (IntoTheRuinsOnceAgain (..), intoTheRuinsOnceAgain) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Deck

newtype IntoTheRuinsOnceAgain = IntoTheRuinsOnceAgain ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheRuinsOnceAgain :: ActCard IntoTheRuinsOnceAgain
intoTheRuinsOnceAgain =
  act
    (1, A)
    IntoTheRuinsOnceAgain
    Cards.intoTheRuinsOnceAgain
    (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance HasAbilities IntoTheRuinsOnceAgain where
  getAbilities (IntoTheRuinsOnceAgain a) = withBaseAbilities a [mkAbility a 1 exploreAction_]

instance RunMessage IntoTheRuinsOnceAgain where
  runMessage msg a@(IntoTheRuinsOnceAgain attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 1
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      chamberOfTime <- getSetAsideCard Locations.chamberOfTime
      pushAll
        [ ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey ExplorationDeck) [chamberOfTime]
        , AddToVictory (toTarget attrs)
        , advanceActDeck attrs
        ]
      pure a
    _ -> IntoTheRuinsOnceAgain <$> runMessage msg attrs
