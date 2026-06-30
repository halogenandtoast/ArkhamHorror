module Arkham.Enemy.Cards.EquineHybridB (equineHybridB) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreTraps)

newtype EquineHybridB = EquineHybridB EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

equineHybridB :: EnemyCard EquineHybridB
equineHybridB =
  enemy EquineHybridB Cards.equineHybridB
    & setSpawnAt (LocationInPosition $ Pos 2 0)

instance HasModifiersFor EquineHybridB where
  getModifiersFor (EquineHybridB a) =
    modifySelf a [IgnoreTraps]

instance HasAbilities EquineHybridB where
  getAbilities (EquineHybridB a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #when AnyDamageEffect (be a) (atLeast 2) AnySource

instance RunMessage EquineHybridB where
  runMessage msg e@(EquineHybridB attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      damageModifier (attrs.ability 1) attrs (MaxDamageTaken AnyDamageEffect 1)
      pure e
    _ -> EquineHybridB <$> liftRunMessage msg attrs
