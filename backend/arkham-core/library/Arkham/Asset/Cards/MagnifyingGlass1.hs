module Arkham.Asset.Cards.MagnifyingGlass1 where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype MagnifyingGlass1 = MagnifyingGlass1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magnifyingGlass1 :: AssetCard MagnifyingGlass1
magnifyingGlass1 = asset MagnifyingGlass1 Cards.magnifyingGlass1

instance HasModifiersFor MagnifyingGlass1 where
  getModifiersFor (InvestigatorTarget iid) (MagnifyingGlass1 a) = pure
    [ toModifier a $ ActionSkillModifier Action.Investigate SkillIntellect 1
    | controlledBy a iid
    ]
  getModifiersFor _ _ = pure []

instance HasAbilities MagnifyingGlass1 where
  getAbilities (MagnifyingGlass1 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> LocationExists (YourLocation <> LocationWithoutClues))
        $ FastAbility Free
    ]

instance RunMessage MagnifyingGlass1 where
  runMessage msg a@(MagnifyingGlass1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (ReturnToHand iid (toTarget attrs))
    _ -> MagnifyingGlass1 <$> runMessage msg attrs
