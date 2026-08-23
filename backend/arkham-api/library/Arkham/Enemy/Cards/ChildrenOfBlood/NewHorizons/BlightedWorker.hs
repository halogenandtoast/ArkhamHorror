module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.BlightedWorker (blightedWorker) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Matcher

newtype BlightedWorker = BlightedWorker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blightedWorker :: EnemyCard BlightedWorker
blightedWorker = enemy BlightedWorker Cards.blightedWorker

instance HasAbilities BlightedWorker where
  getAbilities (BlightedWorker a) =
    extend1 a
      $ restricted a 1 (notExists $ SealedOnInvestigator You #blood)
      $ forced
      $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage BlightedWorker where
  runMessage msg e@(BlightedWorker attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (OnlyInBag #blood) >>= \case
        Just blood -> sealChaosToken iid iid blood
        Nothing -> chooseAndDiscardCard iid (attrs.ability 1)
      pure e
    _ -> BlightedWorker <$> liftRunMessage msg attrs
