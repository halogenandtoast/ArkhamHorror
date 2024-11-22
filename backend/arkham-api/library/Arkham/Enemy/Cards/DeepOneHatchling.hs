module Arkham.Enemy.Cards.DeepOneHatchling (deepOneHatchling, DeepOneHatchling (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Investigator.Projection ()
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Trait (Trait (DeepOne))

newtype DeepOneHatchling = DeepOneHatchling EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneHatchling :: EnemyCard DeepOneHatchling
deepOneHatchling = enemy DeepOneHatchling Cards.deepOneHatchling (1, Static 1, 1) (0, 1)

instance HasAbilities DeepOneHatchling where
  getAbilities (DeepOneHatchling a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)
      , restricted a 2 (exists $ not_ (be a) <> EnemyWithTrait DeepOne <> UnengagedEnemy <> ReadyEnemy)
          $ forced
          $ EnemyDefeated #after You ByAny (be a)
      ]

instance RunMessage DeepOneHatchling where
  runMessage msg e@(DeepOneHatchling attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remainingActions <- iid.remainingActions
      chooseOrRunOneM iid do
        when (remainingActions > 0) do
          labeled "Lose 1 Action" $ push $ LoseActions iid (attrs.ability 1) 1
        labeled "Deep One Hatchling attacks you" $ initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      deepOnes <-
        select
          $ NearestEnemyTo iid (not_ (be attrs) <> EnemyWithTrait DeepOne <> UnengagedEnemy <> ReadyEnemy)
      chooseTargetM iid deepOnes \deepOne -> do
        temporaryModifier deepOne (attrs.ability 2) (RemoveKeyword Aloof) do
          moveTowardsMatching (attrs.ability 2) deepOne (locationWithInvestigator iid)
      pure e
    _ -> DeepOneHatchling <$> liftRunMessage msg attrs
