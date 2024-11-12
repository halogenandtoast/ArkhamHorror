module Arkham.Enemy.Cards.AquaticAbomination (aquaticAbomination, AquaticAbomination (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Matcher

newtype AquaticAbomination = AquaticAbomination EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

aquaticAbomination :: EnemyCard AquaticAbomination
aquaticAbomination = enemy AquaticAbomination Cards.aquaticAbomination (5, Static 7, 2) (2, 2)

instance HasModifiersFor AquaticAbomination where
  getModifiersFor (LocationTarget _) (AquaticAbomination a) = maybeModified a do
    liftGuardM $ a.id <=~> MovingEnemy
    pure [ConnectedToWhen FullyFloodedLocation FullyFloodedLocation]
  getModifiersFor _ _ = pure []

instance RunMessage AquaticAbomination where
  runMessage msg (AquaticAbomination attrs) = runQueueT $ case msg of
    HunterMove eid | eid == attrs.id -> do
      let previouslyMoved = enemyMovedFromHunterKeyword attrs
      attrs' <- liftRunMessage msg attrs
      let newMoved = enemyMovedFromHunterKeyword attrs
      when (not previouslyMoved && newMoved) do
        phaseModifier attrs attrs CannotAttack
      pure $ AquaticAbomination attrs'
    _ -> AquaticAbomination <$> liftRunMessage msg attrs
