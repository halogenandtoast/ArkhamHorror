module Arkham.Enemy.Cards.DeepOneNursemaid (deepOneNursemaid, DeepOneNursemaid (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Trait (Trait (DeepOne))

newtype DeepOneNursemaid = DeepOneNursemaid EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneNursemaid :: EnemyCard DeepOneNursemaid
deepOneNursemaid = enemy DeepOneNursemaid Cards.deepOneNursemaid (0, Static 1, 0) (0, 0)

instance HasModifiersFor DeepOneNursemaid where
  getModifiersFor (EnemyTarget eid) (DeepOneNursemaid a) = maybeModified a do
    guard $ a.id /= eid
    liftGuardM $ a.id <=~> UnengagedEnemy
    liftGuardM $ eid <=~> (EnemyAt (orConnected $ locationWithEnemy a.id) <> EnemyWithTrait DeepOne)
    pure [EnemyFight 1, EnemyEvade 1]
  getModifiersFor _ _ = pure []

instance HasAbilities DeepOneNursemaid where
  getAbilities (DeepOneNursemaid a) = extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

instance RunMessage DeepOneNursemaid where
  runMessage msg e@(DeepOneNursemaid attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mcard <- headMay <$> getEncounterDeck
      for_ mcard \card -> do
        temporaryModifier card (attrs.ability 1) NoSurge do
          drawEncounterCard iid (attrs.ability 1)

      pure e
    _ -> DeepOneNursemaid <$> liftRunMessage msg attrs
