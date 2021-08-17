module Arkham.Types.Asset.Cards.LightningGun5
  ( lightningGun5
  , LightningGun5(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import qualified Arkham.Types.Asset.Uses as Resource
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

instance HasAbilities env LightningGun5 where
  getAbilities _ _ (LightningGun5 a) = pure
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
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
      pure $ LightningGun5 $ attrs & usesL %~ Resource.use
    _ -> LightningGun5 <$> runMessage msg attrs
