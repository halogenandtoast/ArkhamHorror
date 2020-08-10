{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FortyFiveAutomatic where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..))
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import ClassyPrelude
import Lens.Micro

newtype FortyFiveAutomatic = FortyFiveAutomatic Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fortyFiveAutomatic :: AssetId -> FortyFiveAutomatic
fortyFiveAutomatic uuid =
  FortyFiveAutomatic $ (baseAttrs uuid "01016") { assetSlots = [HandSlot] }

instance (AssetRunner env) => RunMessage env FortyFiveAutomatic where
  runMessage msg a@(FortyFiveAutomatic attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      pure
        $ FortyFiveAutomatic
        $ attrs
        & (uses .~ Uses Resource.Ammo 4)
        & (abilities
          .~ [ ( AssetSource aid
               , AssetSource aid
               , 1
               , ActionAbility 1 (Just Action.Fight)
               , NoLimit
               )
             ]
          )
    UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId ->
      case assetUses of
        Uses Resource.Ammo n -> do
          when
            (n == 1)
            (unshiftMessage (RemoveAbilitiesFrom (AssetSource assetId)))
          unshiftMessage
            (ChooseFightEnemy
              iid
              SkillCombat
              [ DamageDealt 1 (AssetSource aid)
              , SkillModifier SkillCombat 1 (AssetSource aid)
              ]
              mempty
              False
            )
          pure $ FortyFiveAutomatic $ attrs & uses .~ Uses Resource.Ammo (n - 1)
        _ -> pure a
    _ -> FortyFiveAutomatic <$> runMessage msg attrs
