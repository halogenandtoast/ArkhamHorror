module Arkham.Types.Asset.Cards.LightningGun5
  ( lightningGun5
  , LightningGun5(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype LightningGun5 = LightningGun5 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightningGun5 :: AssetId -> LightningGun5
lightningGun5 uuid =
  LightningGun5 $ (baseAttrs uuid "02301") { assetSlots = [HandSlot, HandSlot] }

instance ActionRunner env => HasActions env LightningGun5 where
  getActions iid window (LightningGun5 a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility (Just Action.Fight) (ActionCost 1))
          )
      | useCount (assetUses a) > 0 && fightAvailable
      ]
  getActions _ _ _ = pure []

instance HasModifiersFor env LightningGun5 where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env LightningGun5 where
  runMessage msg (LightningGun5 attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      LightningGun5 <$> runMessage msg (attrs & usesL .~ Uses Resource.Ammo 3)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers
          $ toModifiers attrs [DamageDealt 2, SkillModifier SkillCombat 5]
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat False
        ]
      pure $ LightningGun5 $ attrs & usesL %~ Resource.use
    _ -> LightningGun5 <$> runMessage msg attrs
