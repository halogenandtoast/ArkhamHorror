{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Scavenging where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype Scavenging = Scavenging Attrs
  deriving newtype (Show, ToJSON, FromJSON)

scavenging :: AssetId -> Scavenging
scavenging uuid = Scavenging $ baseAttrs uuid "01073"

instance (IsInvestigator investigator) => HasActions env investigator Scavenging where
  getActions i (AfterPassSkillTest You n) (Scavenging Attrs {..})
    | Just (getId () i) == assetInvestigator && n >= 2 = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility
            (AssetSource assetId)
            1
            (ReactionAbility (AfterPassSkillTest You n))
          )
      ]
  getActions i window (Scavenging x) = getActions i window x

instance (AssetRunner env) => RunMessage env Scavenging where
  runMessage msg (Scavenging attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      unshiftMessage (SearchDiscard iid (InvestigatorTarget iid) [Item])
      pure $ Scavenging $ attrs & exhausted .~ True
    _ -> Scavenging <$> runMessage msg attrs
