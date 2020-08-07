{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Rolands38Special where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..))
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import ClassyPrelude
import Lens.Micro

newtype Rolands38SpecialI = Rolands38SpecialI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rolands38Special :: AssetId -> Rolands38SpecialI
rolands38Special uuid =
  Rolands38SpecialI $ (baseAttrs uuid "01006") { assetSlots = [HandSlot] }

instance (AssetRunner env) => RunMessage env Rolands38SpecialI where
  runMessage msg a@(Rolands38SpecialI attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId -> do
      let
        attrs' =
          attrs
            & (uses .~ Uses Resource.Ammo 4)
            & (abilities
              .~ [ ( AssetSource aid
                   , Nothing
                   , 1
                   , ActionAbility 1 (Just Action.Fight)
                   , NoLimit
                   )
                 ]
              )
      Rolands38SpecialI <$> runMessage msg attrs'
    UseCardAbility iid (AssetSource aid, Nothing, 1, _, _) | aid == assetId ->
      case assetUses of
        Uses Resource.Ammo n -> do
          when
            (n == 1)
            (unshiftMessage (RemoveAbilitiesFrom (AssetSource assetId)))
          locationId <- asks (getId @LocationId iid)
          clueCount <- unClueCount <$> asks (getCount locationId)
          let skillModifier = if clueCount == 0 then 1 else 3
          unshiftMessage
            (ChooseFightEnemy
              iid
              SkillCombat
              [ DamageDealt 1 (AssetSource aid)
              , SkillModifier SkillCombat skillModifier (AssetSource aid)
              ]
              mempty
              False
            )
          pure $ Rolands38SpecialI $ attrs & uses .~ Uses Resource.Ammo (n - 1)
        _ -> pure a
    _ -> Rolands38SpecialI <$> runMessage msg attrs
