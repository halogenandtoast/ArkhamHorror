module Arkham.Asset.Cards.PreciousMementoFromAFutureLife4 (PreciousMementoFromAFutureLife4 (..), preciousMementoFromAFutureLife4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher
import Arkham.Prelude

newtype PreciousMementoFromAFutureLife4 = PreciousMementoFromAFutureLife4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preciousMementoFromAFutureLife4 :: AssetCard PreciousMementoFromAFutureLife4
preciousMementoFromAFutureLife4 = assetWith PreciousMementoFromAFutureLife4 Cards.preciousMementoFromAFutureLife4 ((healthL ?~ 3) . (sanityL ?~ 3))

instance HasAbilities PreciousMementoFromAFutureLife4 where
  getAbilities (PreciousMementoFromAFutureLife4 a) =
    [ controlledAbility a 1 (exists $ HealableAsset (toSource a) HorrorType $ AssetWithId (toId a))
        $ ReactionAbility (SkillTestResult #after You AnySkillTest (FailureResult $ atLeast 2)) (exhaust a)
    , controlledAbility a 2 (exists $ HealableAsset (toSource a) DamageType $ AssetWithId (toId a))
        $ ReactionAbility (SkillTestResult #after You AnySkillTest (SuccessResult $ atLeast 2)) (exhaust a)
    ]

instance RunMessage PreciousMementoFromAFutureLife4 where
  runMessage msg a@(PreciousMementoFromAFutureLife4 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ HealHorror (toTarget attrs) (toAbilitySource attrs 1) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ HealDamage (toTarget attrs) (toAbilitySource attrs 1) 1
      pure a
    _ -> PreciousMementoFromAFutureLife4 <$> runMessage msg attrs
