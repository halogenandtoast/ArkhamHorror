module Arkham.Asset.Cards.Pathfinder1 (
  pathfinder1,
  Pathfinder1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Movement

newtype Pathfinder1 = Pathfinder1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathfinder1 :: AssetCard Pathfinder1
pathfinder1 = asset Pathfinder1 Cards.pathfinder1

instance HasAbilities Pathfinder1 where
  getAbilities (Pathfinder1 attrs) =
    [ restrictedAbility
        attrs
        1
        ( ControlsThis
            <> InvestigatorExists (You <> UnengagedInvestigator)
            <> LocationExists AccessibleLocation
        )
        (FastAbility $ exhaust attrs)
    ]

instance RunMessage Pathfinder1 where
  runMessage msg a@(Pathfinder1 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      locations <- accessibleLocations iid
      push
        $ chooseOne iid
        $ [targetLabel lid [Move $ move (toAbilitySource attrs 1) iid lid] | lid <- locations]
      pure a
    _ -> Pathfinder1 <$> runMessage msg attrs
