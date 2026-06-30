module Arkham.Enemy.Cards.EquineHybridC (equineHybridC) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreTraps)

newtype EquineHybridC = EquineHybridC EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

equineHybridC :: EnemyCard EquineHybridC
equineHybridC =
  enemy EquineHybridC Cards.equineHybridC
    & setSpawnAt (LocationInPosition $ Pos (-3) 0)

instance HasModifiersFor EquineHybridC where
  getModifiersFor (EquineHybridC a) =
    modifySelf a [IgnoreTraps]

instance HasAbilities EquineHybridC where
  getAbilities (EquineHybridC a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #when AnyDamageEffect (be a) (atLeast 2) AnySource

instance RunMessage EquineHybridC where
  runMessage msg e@(EquineHybridC attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      damageModifier (attrs.ability 1) attrs (MaxDamageTaken AnyDamageEffect 1)
      pure e
    _ -> EquineHybridC <$> liftRunMessage msg attrs
