module Arkham.Asset.Cards.Lantern (
  lantern,
  Lantern (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Lantern = Lantern AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

lantern :: AssetCard Lantern
lantern = asset Lantern Cards.lantern

instance HasAbilities Lantern where
  getAbilities (Lantern x) =
    [ investigateAbility x 1 mempty ControlsThis
    , doesNotProvokeAttacksOfOpportunity
        $ restrictedAbility x 2 (ControlsThis <> CanDealDamage <> enemyExists (EnemyAt YourLocation))
        $ actionAbilityWithCost (discardCost x)
    ]

instance RunMessage Lantern where
  runMessage msg a@(Lantern attrs) = case msg of
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
      enemies <- selectList $ enemyAtLocationWith iid
      player <- getPlayer iid
      push
        $ chooseOne player [targetLabel enemy [EnemyDamage enemy $ nonAttack source 1] | enemy <- enemies]
      pure a
    _ -> Lantern <$> runMessage msg attrs
