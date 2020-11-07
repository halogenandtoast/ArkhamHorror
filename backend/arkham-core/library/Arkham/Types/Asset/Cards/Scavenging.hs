{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Scavenging where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype Scavenging = Scavenging Attrs
  deriving newtype (Show, ToJSON, FromJSON)

scavenging :: AssetId -> Scavenging
scavenging uuid = Scavenging $ baseAttrs uuid "01073" $ pure ()

instance HasModifiersFor env Scavenging where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Scavenging where
  getActions iid (AfterPassSkillTest You n) (Scavenging a)
    | ownedBy a iid && n >= 2 = do
      discard <- getDiscardOf iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource a)
              1
              (ReactionAbility (AfterPassSkillTest You n))
            )
        | any ((Item `member`) . getTraits) discard
        ]
  getActions i window (Scavenging x) = getActions i window x

instance AssetRunner env => RunMessage env Scavenging where
  runMessage msg (Scavenging attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage (SearchDiscard iid (InvestigatorTarget iid) [Item])
      pure $ Scavenging $ attrs & exhausted .~ True
    _ -> Scavenging <$> runMessage msg attrs
