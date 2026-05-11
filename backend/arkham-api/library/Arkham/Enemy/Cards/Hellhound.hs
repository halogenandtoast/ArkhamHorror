module Arkham.Enemy.Cards.Hellhound (hellhound) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype Hellhound = Hellhound EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hellhound :: EnemyCard Hellhound
hellhound = enemy Hellhound Cards.hellhound (2, Static 3, 4) (1, 1)

instance HasAbilities Hellhound where
  getAbilities (Hellhound a) =
    extend1 a
      $ restricted a 1 (DuringPhase #enemy)
      $ forced
      $ EnemyAttacks #after (You <> ControlsAsset DiscardableAsset) AnyEnemyAttack (be a)

instance RunMessage Hellhound where
  runMessage msg a@(Hellhound attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardAsset iid attrs
      pure a
    _ -> Hellhound <$> liftRunMessage msg attrs
