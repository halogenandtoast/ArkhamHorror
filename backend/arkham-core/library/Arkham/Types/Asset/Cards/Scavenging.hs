{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Scavenging where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype Scavenging = Scavenging Attrs
  deriving newtype (Show, ToJSON, FromJSON)

scavenging :: AssetId -> Scavenging
scavenging uuid = Scavenging $ baseAttrs uuid "01073"

instance HasModifiersFor env investigator Scavenging where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator Scavenging where
  getActions i (AfterPassSkillTest You n) (Scavenging a)
    | ownedBy a i && n >= 2 = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (toSource a) 1 (ReactionAbility (AfterPassSkillTest You n))
          )
      | any ((Item `member`) . getTraits) (discardOf i)
      ]
  getActions i window (Scavenging x) = getActions i window x

instance (AssetRunner env) => RunMessage env Scavenging where
  runMessage msg (Scavenging attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage (SearchDiscard iid (InvestigatorTarget iid) [Item])
      pure $ Scavenging $ attrs & exhausted .~ True
    _ -> Scavenging <$> runMessage msg attrs
