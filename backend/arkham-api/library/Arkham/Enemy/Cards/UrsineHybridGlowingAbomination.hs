module Arkham.Enemy.Cards.UrsineHybridGlowingAbomination (ursineHybridGlowingAbomination) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype UrsineHybridGlowingAbomination = UrsineHybridGlowingAbomination EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ursineHybridGlowingAbomination :: EnemyCard UrsineHybridGlowingAbomination
ursineHybridGlowingAbomination =
  enemy UrsineHybridGlowingAbomination Cards.ursineHybridGlowingAbomination (5, Static 5, 3) (3, 2)

instance HasModifiersFor UrsineHybridGlowingAbomination where
  getModifiersFor (UrsineHybridGlowingAbomination a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities UrsineHybridGlowingAbomination where
  getAbilities (UrsineHybridGlowingAbomination a) =
    extend1 a $ restricted a 1 (OnAct 2) $ forced $ EnemyWouldBeDefeated #when (be a)

instance RunMessage UrsineHybridGlowingAbomination where
  runMessage msg (UrsineHybridGlowingAbomination attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      healAllDamage (attrs.ability 1) attrs
      place attrs $ OutOfPlay PursuitZone
      pure $ UrsineHybridGlowingAbomination $ attrs & exhaustedL .~ False
    _ -> UrsineHybridGlowingAbomination <$> liftRunMessage msg attrs
