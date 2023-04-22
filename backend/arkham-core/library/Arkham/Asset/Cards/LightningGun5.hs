module Arkham.Asset.Cards.LightningGun5
  ( lightningGun5
  , LightningGun5(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype LightningGun5 = LightningGun5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightningGun5 :: AssetCard LightningGun5
lightningGun5 = asset LightningGun5 Cards.lightningGun5

instance HasAbilities LightningGun5 where
  getAbilities (LightningGun5 a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Ammo 1])
    ]

instance RunMessage LightningGun5 where
  runMessage msg a@(LightningGun5 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 2, SkillModifier SkillCombat 5]
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
    _ -> LightningGun5 <$> runMessage msg attrs
