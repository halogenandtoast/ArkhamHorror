{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Scavenging where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype Scavenging = Scavenging Attrs
  deriving newtype (Show, ToJSON, FromJSON)

scavenging :: AssetId -> Scavenging
scavenging uuid = Scavenging $ baseAttrs uuid "01073"

instance HasModifiersFor env Scavenging where
  getModifiersFor = noModifiersFor

ability :: Window -> Attrs -> Ability
ability window a =
  mkAbility (toSource a) 1 (ReactionAbility window $ ExhaustCost (toTarget a))

instance ActionRunner env => HasActions env Scavenging where
  getActions iid window@(AfterPassSkillTest (SkillTestSource _ _ _ (Just Action.Investigate)) You n) (Scavenging a)
    | ownedBy a iid && n >= 2
    = do
      discard <- getDiscardOf iid
      pure
        [ ActivateCardAbilityAction iid (ability window a)
        | any ((Item `member`) . getTraits) discard
        ]
  getActions i window (Scavenging x) = getActions i window x

instance AssetRunner env => RunMessage env Scavenging where
  runMessage msg a@(Scavenging attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessage (SearchDiscard iid (InvestigatorTarget iid) [Item])
    _ -> Scavenging <$> runMessage msg attrs
