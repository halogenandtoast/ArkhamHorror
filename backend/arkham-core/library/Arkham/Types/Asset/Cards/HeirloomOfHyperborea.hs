module Arkham.Types.Asset.Cards.HeirloomOfHyperborea where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Matcher qualified as Matcher
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait

newtype HeirloomOfHyperborea = HeirloomOfHyperborea AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperborea :: AssetCard HeirloomOfHyperborea
heirloomOfHyperborea = asset HeirloomOfHyperborea Cards.heirloomOfHyperborea

instance HasAbilities HeirloomOfHyperborea where
  getAbilities (HeirloomOfHyperborea x) =
    [ restrictedAbility x 1 (OwnsThis <> CanDrawCards) $ ReactionAbility
        (Matcher.PlayCard
          Timing.After
          You
          (BasicCardMatch $ CardWithTrait Spell)
        )
        Free
    ]

instance AssetRunner env => RunMessage env HeirloomOfHyperborea where
  runMessage msg a@(HeirloomOfHyperborea attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> HeirloomOfHyperborea <$> runMessage msg attrs
