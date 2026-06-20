module Arkham.Enemy.Cards.MoltingHybridB (moltingHybridB) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreBarriers)

newtype MoltingHybridB = MoltingHybridB EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moltingHybridB :: EnemyCard MoltingHybridB
moltingHybridB =
  enemy MoltingHybridB Cards.moltingHybridB
    & setSpawnAt (LocationInPosition $ Pos 0 (-2))

instance HasModifiersFor MoltingHybridB where
  getModifiersFor (MoltingHybridB a) =
    modifySelf a [IgnoreBarriers]

instance HasAbilities MoltingHybridB where
  getAbilities (MoltingHybridB a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #when AnyDamageEffect (be a) AnyValue AnySource

instance RunMessage MoltingHybridB where
  runMessage msg e@(MoltingHybridB attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      damageModifier (attrs.ability 1) attrs (MaxDamageTaken AnyDamageEffect 1)
      pure e
    _ -> MoltingHybridB <$> liftRunMessage msg attrs
