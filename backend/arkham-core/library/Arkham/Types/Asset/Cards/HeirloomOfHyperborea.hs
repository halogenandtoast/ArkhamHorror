module Arkham.Types.Asset.Cards.HeirloomOfHyperborea where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message hiding (PlayCard)
import Arkham.Types.Restriction
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype HeirloomOfHyperborea = HeirloomOfHyperborea AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperborea :: AssetCard HeirloomOfHyperborea
heirloomOfHyperborea =
  accessory HeirloomOfHyperborea Cards.heirloomOfHyperborea

instance HasModifiersFor env HeirloomOfHyperborea

instance HasActions HeirloomOfHyperborea where
  getActions (HeirloomOfHyperborea x) =
    [ restrictedAbility x 1 OwnsThis $ ReactionAbility
        (PlayCard Timing.After You (BasicCardMatch $ CardWithTrait Spell))
        Free
    ]

instance (AssetRunner env) => RunMessage env HeirloomOfHyperborea where
  runMessage msg a@(HeirloomOfHyperborea attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> HeirloomOfHyperborea <$> runMessage msg attrs
