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
deepOneNursemaid = enemy DeepOneNursemaid Cards.deepOneNursemaid (3, Static 2, 2) (1, 1)

instance HasModifiersFor DeepOneNursemaid where
  getModifiersFor (DeepOneNursemaid a) = do
    whenM (a.id <=~> UnengagedEnemy) do
      modifySelect
        a
        (not_ (be a) <> at_ (orConnected $ locationWithEnemy a.id) <> EnemyWithTrait DeepOne)
        [EnemyFight 1, EnemyEvade 1]

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
