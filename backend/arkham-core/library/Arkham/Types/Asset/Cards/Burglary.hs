{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Burglary where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype Burglary = Burglary Attrs
  deriving newtype (Show, ToJSON, FromJSON)

burglary :: AssetId -> Burglary
burglary uuid = Burglary $ baseAttrs uuid "01045"

instance (IsInvestigator investigator) => HasActions env investigator Burglary where
  getActions i NonFast (Burglary Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility
            (AssetSource assetId)
            1
            (ActionAbility 1 (Just Action.Investigate))
          )
      | not assetExhausted && hasActionsRemaining i
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Burglary where
  runMessage msg (Burglary attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      lid <- asks (getId @LocationId iid)
      unshiftMessage
        (Investigate
          iid
          lid
          SkillIntellect
          mempty
          [TakeResources iid 3 False]
          False
        )
      pure $ Burglary $ attrs & exhausted .~ True
    _ -> Burglary <$> runMessage msg attrs
