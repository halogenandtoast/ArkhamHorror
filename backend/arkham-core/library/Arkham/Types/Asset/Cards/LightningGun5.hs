module Arkham.Types.Asset.Cards.LightningGun5
  ( lightningGun5
  , LightningGun5(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses qualified as Resource
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target

newtype LightningGun5 = LightningGun5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightningGun5 :: AssetCard LightningGun5
lightningGun5 =
  assetWith LightningGun5 Cards.lightningGun5 (slotsL .~ [HandSlot, HandSlot])

instance HasAbilities LightningGun5 where
  getAbilities (LightningGun5 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (toId a) Resource.Ammo 1])
    ]

instance AssetRunner env => RunMessage env LightningGun5 where
  runMessage msg (LightningGun5 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 2, SkillModifier SkillCombat 5]
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure $ LightningGun5 $ attrs & usesL %~ Resource.use
    _ -> LightningGun5 <$> runMessage msg attrs
