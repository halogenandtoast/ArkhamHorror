module Arkham.Enemy.Cards.MoltingHybridA (moltingHybridA) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreBarriers)

newtype MoltingHybridA = MoltingHybridA EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moltingHybridA :: EnemyCard MoltingHybridA
moltingHybridA =
  enemy MoltingHybridA Cards.moltingHybridA
    & setSpawnAt (LocationInPosition $ Pos 2 0)

instance HasModifiersFor MoltingHybridA where
  getModifiersFor (MoltingHybridA a) =
    modifySelf a [IgnoreBarriers]

instance HasAbilities MoltingHybridA where
  getAbilities (MoltingHybridA a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #when AnyDamageEffect (be a) AnyValue AnySource

instance RunMessage MoltingHybridA where
  runMessage msg e@(MoltingHybridA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      damageModifier (attrs.ability 1) attrs (MaxDamageTaken AnyDamageEffect 1)
      pure e
    _ -> MoltingHybridA <$> liftRunMessage msg attrs
