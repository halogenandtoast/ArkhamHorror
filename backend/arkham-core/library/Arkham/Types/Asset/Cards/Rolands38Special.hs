{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Rolands38Special where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
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

newtype Rolands38Special = Rolands38Special Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rolands38Special :: AssetId -> Rolands38Special
rolands38Special uuid =
  Rolands38Special $ (baseAttrs uuid "01006") { assetSlots = [HandSlot] }

instance (AssetRunner env, IsInvestigator investigator) => HasActions env investigator Rolands38Special where
  getActions i (Rolands38Special Attrs {..}) = do
    fightAvailable <- hasFightActions i
    pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility
            (AssetSource assetId)
            1
            (ActionAbility 1 (Just Action.Fight))
          )
      | fightAvailable
      ]

instance (AssetRunner env) => RunMessage env Rolands38Special where
  runMessage msg a@(Rolands38Special attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId -> do
      let
        attrs' =
          attrs
            & (uses .~ Uses Resource.Ammo 4)
            & (abilities
              .~ [ mkAbility
                     (AssetSource aid)
                     1
                     (ActionAbility 1 (Just Action.Fight))
                 ]
              )
      Rolands38Special <$> runMessage msg attrs'
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
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
          pure $ Rolands38Special $ attrs & uses .~ Uses Resource.Ammo (n - 1)
        _ -> pure a
    _ -> Rolands38Special <$> runMessage msg attrs
