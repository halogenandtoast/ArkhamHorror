module Arkham.Types.Asset.Cards.HeirloomOfHyperborea where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Trait
import Arkham.Types.Window

newtype HeirloomOfHyperborea = HeirloomOfHyperborea AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperborea :: AssetCard HeirloomOfHyperborea
heirloomOfHyperborea =
  accessory HeirloomOfHyperborea Cards.heirloomOfHyperborea

instance HasModifiersFor env HeirloomOfHyperborea

reactionAbility :: AssetAttrs -> Ability
reactionAbility attrs = mkAbility (toSource attrs) 1 (LegacyReactionAbility Free)

instance HasAbilities env HeirloomOfHyperborea where
  getAbilities iid (AfterPlayCard who card) (HeirloomOfHyperborea a)
    | ownedBy a iid && iid == who = pure
      [ reactionAbility a | Spell `elem` toTraits card ]
  getAbilities i window (HeirloomOfHyperborea x) = getAbilities i window x

instance (AssetRunner env) => RunMessage env HeirloomOfHyperborea where
  runMessage msg a@(HeirloomOfHyperborea attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> HeirloomOfHyperborea <$> runMessage msg attrs
