module Arkham.Asset.Cards.Shrivelling (
  Shrivelling (..),
  shrivelling,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype Shrivelling = Shrivelling AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrivelling :: AssetCard Shrivelling
shrivelling = asset Shrivelling Cards.shrivelling

instance HasAbilities Shrivelling where
  getAbilities (Shrivelling a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill (Just Action.Fight) #willpower
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1]
    ]

instance RunMessage Shrivelling where
  runMessage msg a@(Shrivelling attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers (toAbilitySource attrs 1) iid [DamageDealt 1]
        , CreateEffect "01060" Nothing (toAbilitySource attrs 1) (toTarget iid)
        , chooseFightEnemy iid (toAbilitySource attrs 1) #willpower
        ]
      pure a
    _ -> Shrivelling <$> runMessage msg attrs
