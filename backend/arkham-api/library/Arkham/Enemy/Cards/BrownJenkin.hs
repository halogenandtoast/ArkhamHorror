module Arkham.Enemy.Cards.BrownJenkin (brownJenkin) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait (Trait (Creature))

newtype BrownJenkin = BrownJenkin EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brownJenkin :: EnemyCard BrownJenkin
brownJenkin = enemy BrownJenkin Cards.brownJenkin (1, Static 1, 4) (1, 1)

instance HasModifiersFor BrownJenkin where
  getModifiersFor (BrownJenkin attrs) =
    modifySelect attrs (ReadyEnemy <> EnemyWithTrait Creature) [EnemyFight 2]

instance HasAbilities BrownJenkin where
  getAbilities (BrownJenkin x) =
    extend1 x
      $ restricted x 1 (thisExists x ReadyEnemy <> exists (at_ (locationWithEnemy x) <> HandWith AnyCards))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage BrownJenkin where
  runMessage msg e@(BrownJenkin attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ investigator_ $ at_ (locationWithEnemy attrs)
      for_ investigators \iid -> do
        handCount <- selectCount $ inHandOf NotForPlay iid <> not_ HiddenInHandCard
        when (handCount > 0) do
          batched \_ -> do
            push $ DiscardHand iid (toSource attrs)
            drawCards iid (attrs.ability 1) handCount
      pure e
    _ -> BrownJenkin <$> liftRunMessage msg attrs
