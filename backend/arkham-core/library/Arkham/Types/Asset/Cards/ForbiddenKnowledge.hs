{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ForbiddenKnowledge where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype ForbiddenKnowledge = ForbiddenKnowledge Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

forbiddenKnowledge :: AssetId -> ForbiddenKnowledge
forbiddenKnowledge uuid = ForbiddenKnowledge $ baseAttrs uuid "01058" $ pure ()

instance HasModifiersFor env ForbiddenKnowledge where
  getModifiersFor _ _ _ = pure []

instance HasActions env ForbiddenKnowledge where
  getActions iid window (ForbiddenKnowledge a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (FastAbility window))
    | useCount (assetUses a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ForbiddenKnowledge where
  runMessage msg (ForbiddenKnowledge attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      ForbiddenKnowledge
        <$> runMessage msg (attrs & uses .~ Uses Resource.Secret 4)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessages
        [InvestigatorAssignDamage iid source 0 1, TakeResources iid 1 False]
      pure $ ForbiddenKnowledge $ attrs & uses %~ Resource.use
    _ -> ForbiddenKnowledge <$> runMessage msg attrs
