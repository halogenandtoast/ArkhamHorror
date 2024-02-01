module Arkham.Asset.Cards.Lantern2 (
  lantern2,
  Lantern2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype Lantern2 = Lantern2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

lantern2 :: AssetCard Lantern2
lantern2 = asset Lantern2 Cards.lantern2

instance HasAbilities Lantern2 where
  getAbilities (Lantern2 x) =
    [ investigateAbility x 1 mempty ControlsThis
    , doesNotProvokeAttacksOfOpportunity
        $ controlledAbility x 2 (exists $ EnemyAt YourLocation)
        $ actionAbilityWithCost (OrCost [discardCost x, removeCost x])
    ]

instance RunMessage Lantern2 where
  runMessage msg a@(Lantern2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- fieldJust InvestigatorLocation iid
      let source = toAbilitySource attrs 1
      investigation <- mkInvestigate iid source
      pushAll
        [ skillTestModifier source lid (ShroudModifier (-1))
        , toMessage investigation
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = toAbilitySource attrs 2
      let
        n =
          case assetPlacement attrs of
            OutOfPlay RemovedZone -> 2
            _ -> 1
      enemies <- selectList $ enemyAtLocationWith iid
      player <- getPlayer iid
      push
        $ chooseOne player [targetLabel enemy [EnemyDamage enemy $ nonAttack source n] | enemy <- enemies]
      pure a
    _ -> Lantern2 <$> runMessage msg attrs
