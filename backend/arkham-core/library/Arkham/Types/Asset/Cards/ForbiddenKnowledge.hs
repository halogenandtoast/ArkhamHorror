{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ForbiddenKnowledge where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Source
import ClassyPrelude
import Lens.Micro

newtype ForbiddenKnowledge = ForbiddenKnowledge Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

forbiddenKnowledge :: AssetId -> ForbiddenKnowledge
forbiddenKnowledge uuid = ForbiddenKnowledge $ baseAttrs uuid "01058"

instance (ActionRunner env investigator) => HasActions env investigator ForbiddenKnowledge where
  getActions i window (ForbiddenKnowledge Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (AssetSource assetId) 1 (FastAbility window))
      | useCount assetUses > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ForbiddenKnowledge where
  runMessage msg a@(ForbiddenKnowledge attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      ForbiddenKnowledge
        <$> runMessage msg (attrs & uses .~ Uses Resource.Secret 4)
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
      case assetUses of
        Uses Resource.Secret n -> do
          unshiftMessages
            [ InvestigatorAssignDamage iid (AssetSource aid) 0 1
            , TakeResources iid 1 False
            ]
          pure $ ForbiddenKnowledge $ attrs & uses .~ Uses
            Resource.Charge
            (n - 1)
        _ -> pure a
    _ -> ForbiddenKnowledge <$> runMessage msg attrs
