module Arkham.Enemy.Cards.MoltingHybridC (moltingHybridC) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreBarriers)

newtype MoltingHybridC = MoltingHybridC EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moltingHybridC :: EnemyCard MoltingHybridC
moltingHybridC =
  enemy MoltingHybridC Cards.moltingHybridC (2, Static 3, 4) (0, 1)
    & setSpawnAt (LocationInPosition $ Pos (-3) 0)

instance HasModifiersFor MoltingHybridC where
  getModifiersFor (MoltingHybridC a) =
    modifySelf a [IgnoreBarriers]

instance HasAbilities MoltingHybridC where
  getAbilities (MoltingHybridC a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #when AnyDamageEffect (be a) AnyValue AnySource

instance RunMessage MoltingHybridC where
  runMessage msg e@(MoltingHybridC attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      damageModifier (attrs.ability 1) attrs (MaxDamageTaken AnyDamageEffect 1)
      pure e
    _ -> MoltingHybridC <$> liftRunMessage msg attrs
