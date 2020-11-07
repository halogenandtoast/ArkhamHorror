{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.JennysTwin45s where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype JennysTwin45s = JennysTwin45s Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jennysTwin45s :: AssetId -> JennysTwin45s
jennysTwin45s uuid =
  JennysTwin45s $ baseAttrs uuid "02010" $ slots .= [HandSlot, HandSlot]

instance HasModifiersFor env JennysTwin45s where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env JennysTwin45s where
  getActions iid window (JennysTwin45s a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 (Just Action.Fight)))
      | useCount (assetUses a) > 0 && fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env JennysTwin45s where
  runMessage msg (JennysTwin45s attrs) = case msg of
    InvestigatorPlayDynamicAsset _ aid _ _ n | aid == assetId attrs ->
      JennysTwin45s <$> runMessage msg (attrs & uses .~ Uses Resource.Ammo n)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage $ ChooseFightEnemy
        iid
        source
        SkillCombat
        [DamageDealt 1, SkillModifier SkillCombat 2]
        mempty
        False
      pure $ JennysTwin45s $ attrs & uses %~ Resource.use
    _ -> JennysTwin45s <$> runMessage msg attrs
