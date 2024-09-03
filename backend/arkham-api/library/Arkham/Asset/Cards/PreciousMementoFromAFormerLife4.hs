module Arkham.Asset.Cards.PreciousMementoFromAFormerLife4 (PreciousMementoFromAFormerLife4 (..), preciousMementoFromAFormerLife4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher
import Arkham.Prelude

newtype PreciousMementoFromAFormerLife4 = PreciousMementoFromAFormerLife4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preciousMementoFromAFormerLife4 :: AssetCard PreciousMementoFromAFormerLife4
preciousMementoFromAFormerLife4 = assetWith PreciousMementoFromAFormerLife4 Cards.preciousMementoFromAFormerLife4 ((healthL ?~ 3) . (sanityL ?~ 3))

instance HasAbilities PreciousMementoFromAFormerLife4 where
  getAbilities (PreciousMementoFromAFormerLife4 a) =
    [ controlledAbility a 1 (exists $ HealableAsset (toSource a) DamageType $ AssetWithId (toId a))
        $ ReactionAbility (SkillTestResult #after You AnySkillTest (FailureResult $ atLeast 2)) (exhaust a)
    , controlledAbility a 2 (exists $ HealableAsset (toSource a) HorrorType $ AssetWithId (toId a))
        $ ReactionAbility (SkillTestResult #after You AnySkillTest (SuccessResult $ atLeast 2)) (exhaust a)
    ]

instance RunMessage PreciousMementoFromAFormerLife4 where
  runMessage msg a@(PreciousMementoFromAFormerLife4 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ HealDamage (toTarget attrs) (toAbilitySource attrs 1) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ HealHorror (toTarget attrs) (toAbilitySource attrs 1) 1
      pure a
    _ -> PreciousMementoFromAFormerLife4 <$> runMessage msg attrs
