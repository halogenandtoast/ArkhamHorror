module Arkham.Asset.Cards.ParadoxicalCovenant2 (paradoxicalCovenant2, ParadoxicalCovenant2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTest.Step

newtype ParadoxicalCovenant2 = ParadoxicalCovenant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradoxicalCovenant2 :: AssetCard ParadoxicalCovenant2
paradoxicalCovenant2 = asset ParadoxicalCovenant2 Cards.paradoxicalCovenant2

instance HasAbilities ParadoxicalCovenant2 where
  getAbilities (ParadoxicalCovenant2 attrs) =
    [ controlledAbility
        attrs
        1
        (DuringSkillTest (SkillTestWithRevealedChaosToken #bless <> SkillTestWithRevealedChaosToken #curse))
        $ ReactionAbility (SkillTestStep #after RevealChaosTokenStep) (exhaust attrs)
    ]

instance RunMessage ParadoxicalCovenant2 where
  runMessage msg a@(ParadoxicalCovenant2 attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push PassSkillTest
      pure a
    _ -> ParadoxicalCovenant2 <$> runMessage msg attrs
