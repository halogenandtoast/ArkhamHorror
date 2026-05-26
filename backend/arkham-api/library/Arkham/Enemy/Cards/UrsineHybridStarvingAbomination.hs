module Arkham.Enemy.Cards.UrsineHybridStarvingAbomination (ursineHybridStarvingAbomination) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf, modifySelectWhen)
import Arkham.Matcher
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreBarriers, pattern IgnoreDecoys)
import Arkham.Trait (Trait (Field))

newtype UrsineHybridStarvingAbomination = UrsineHybridStarvingAbomination EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ursineHybridStarvingAbomination :: EnemyCard UrsineHybridStarvingAbomination
ursineHybridStarvingAbomination =
  enemy UrsineHybridStarvingAbomination Cards.ursineHybridStarvingAbomination (5, Static 6, 3) (3, 2)

instance HasModifiersFor UrsineHybridStarvingAbomination where
  getModifiersFor (UrsineHybridStarvingAbomination a) = do
    n <- perPlayer 3
    modifySelf a [HealthModifier n, IgnoreBarriers, IgnoreDecoys]
    isMoving <- a.id <=~> MovingEnemy
    modifySelectWhen a isMoving Anywhere [ConnectedToWhen (LocationWithTrait Field) (LocationWithTrait Field)]

instance RunMessage UrsineHybridStarvingAbomination where
  runMessage msg (UrsineHybridStarvingAbomination attrs) = runQueueT $ case msg of
    _ -> UrsineHybridStarvingAbomination <$> liftRunMessage msg attrs
