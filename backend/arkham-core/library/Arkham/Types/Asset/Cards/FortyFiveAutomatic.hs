{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FortyFiveAutomatic where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype FortyFiveAutomatic = FortyFiveAutomatic Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fortyFiveAutomatic :: AssetId -> FortyFiveAutomatic
fortyFiveAutomatic uuid =
  FortyFiveAutomatic $ baseAttrs uuid "01016" $ slots .= [HandSlot]

instance HasModifiersFor env FortyFiveAutomatic where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env FortyFiveAutomatic where
  getActions iid window (FortyFiveAutomatic a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 (Just Action.Fight)))
      | useCount (assetUses a) > 0 && fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env FortyFiveAutomatic where
  runMessage msg (FortyFiveAutomatic attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      FortyFiveAutomatic
        <$> runMessage msg (attrs & uses .~ Uses Resource.Ammo 4)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage $ ChooseFightEnemy
        iid
        source
        SkillCombat
        [DamageDealt 1, SkillModifier SkillCombat 1]
        mempty
        False
      pure $ FortyFiveAutomatic $ attrs & uses %~ Resource.use
    _ -> FortyFiveAutomatic <$> runMessage msg attrs
