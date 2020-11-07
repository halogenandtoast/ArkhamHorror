{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FortyOneDerringer where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype FortyOneDerringer = FortyOneDerringer Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fortyOneDerringer :: AssetId -> FortyOneDerringer
fortyOneDerringer uuid =
  FortyOneDerringer $ baseAttrs uuid "01047" $ slots .= [HandSlot]

instance HasModifiersFor env FortyOneDerringer where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env FortyOneDerringer where
  getActions iid window (FortyOneDerringer a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 (Just Action.Fight)))
      | useCount (assetUses a) > 0 && fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env FortyOneDerringer where
  runMessage msg (FortyOneDerringer attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      FortyOneDerringer
        <$> runMessage msg (attrs & uses .~ Uses Resource.Ammo 3)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage $ ChooseFightEnemy
        iid
        source
        SkillCombat
        [ModifierIfSucceededBy 2 (DamageDealt 1), SkillModifier SkillCombat 2]
        mempty
        False
      pure $ FortyOneDerringer $ attrs & uses %~ Resource.use
    _ -> FortyOneDerringer <$> runMessage msg attrs
