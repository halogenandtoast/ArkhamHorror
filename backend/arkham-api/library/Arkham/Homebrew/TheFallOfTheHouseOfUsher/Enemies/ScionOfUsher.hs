module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Enemies.ScionOfUsher (scionOfUsher) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Relic, Spell))

newtype ScionOfUsher = ScionOfUsher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scionOfUsher :: EnemyCard ScionOfUsher
scionOfUsher = enemy ScionOfUsher Cards.scionOfUsher

instance HasAbilities ScionOfUsher where
  getAbilities (ScionOfUsher a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ EnemyTakeDamage #when AnyDamageEffect (be a) AnyValue
          $ NotSource
          $ SourceMatchesAny [SourceWithTrait Spell, SourceWithTrait Relic]
      , mkAbility a 2
          $ freeReaction
          $ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) (SuccessResult $ atLeast 3)
      ]

instance RunMessage ScionOfUsher where
  runMessage msg e@(ScionOfUsher attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- A cap, not a reduction: 0 damage stays 0.
      damageModifier (attrs.ability 1) attrs (MaxDamageTaken AnyDamageEffect 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      defeatEnemy attrs iid (attrs.ability 2)
      pure e
    _ -> ScionOfUsher <$> liftRunMessage msg attrs
