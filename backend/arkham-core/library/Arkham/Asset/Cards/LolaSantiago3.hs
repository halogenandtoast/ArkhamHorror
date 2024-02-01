module Arkham.Asset.Cards.LolaSantiago3 (
  lolaSantiago3,
  LolaSantiago3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost.FieldCost
import Arkham.Discover
import Arkham.Location.Types (Field (..))
import Arkham.Matcher

newtype LolaSantiago3 = LolaSantiago3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

lolaSantiago3 :: AssetCard LolaSantiago3
lolaSantiago3 = ally LolaSantiago3 Cards.lolaSantiago3 (2, 2)

instance HasModifiersFor LolaSantiago3 where
  getModifiersFor (InvestigatorTarget iid) (LolaSantiago3 a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #intellect 1, SkillModifier #agility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities LolaSantiago3 where
  getAbilities (LolaSantiago3 a) =
    [ controlledAbility a 1 ClueOnLocation
        $ FastAbility (exhaust a <> FieldResourceCost (FieldCost YourLocation LocationShroud))
    ]

instance RunMessage LolaSantiago3 where
  runMessage msg a@(LolaSantiago3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ discoverAtYourLocation iid (toAbilitySource attrs 1) 1
      pure a
    _ -> LolaSantiago3 <$> runMessage msg attrs
