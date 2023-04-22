module Arkham.Asset.Cards.LolaSantiago3
  ( lolaSantiago3
  , LolaSantiago3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Location.Types ( Field (..) )
import Arkham.Cost.FieldCost
import Arkham.Matcher
import Arkham.SkillType

newtype LolaSantiago3 = LolaSantiago3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lolaSantiago3 :: AssetCard LolaSantiago3
lolaSantiago3 = ally LolaSantiago3 Cards.lolaSantiago3 (2, 2)

instance HasModifiersFor LolaSantiago3 where
  getModifiersFor (InvestigatorTarget iid) (LolaSantiago3 a)
    | controlledBy a iid = pure $ toModifiers
      a
      [SkillModifier SkillIntellect 1, SkillModifier SkillAgility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities LolaSantiago3 where
  getAbilities (LolaSantiago3 a) =
    [ restrictedAbility a 1 (ControlsThis <> ClueOnLocation)
        $ FastAbility
        $ ExhaustCost (toTarget a)
        <> FieldResourceCost (FieldCost YourLocation LocationShroud)
    ]

instance RunMessage LolaSantiago3 where
  runMessage msg a@(LolaSantiago3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid 1 Nothing
      pure a
    _ -> LolaSantiago3 <$> runMessage msg attrs
