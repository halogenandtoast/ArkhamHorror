module Arkham.Enemy.Cards.EquineHybridA (equineHybridA) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreTraps)

newtype EquineHybridA = EquineHybridA EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

equineHybridA :: EnemyCard EquineHybridA
equineHybridA =
  enemy EquineHybridA Cards.equineHybridA
    & setSpawnAt (LocationInPosition $ Pos 0 2)

instance HasModifiersFor EquineHybridA where
  getModifiersFor (EquineHybridA a) =
    modifySelf a [IgnoreTraps]

instance HasAbilities EquineHybridA where
  getAbilities (EquineHybridA a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #when AnyDamageEffect (be a) (atLeast 2) AnySource

instance RunMessage EquineHybridA where
  runMessage msg e@(EquineHybridA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      damageModifier (attrs.ability 1) attrs (MaxDamageTaken AnyDamageEffect 1)
      pure e
    _ -> EquineHybridA <$> liftRunMessage msg attrs
