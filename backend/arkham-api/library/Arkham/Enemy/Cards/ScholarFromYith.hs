module Arkham.Enemy.Cards.ScholarFromYith (scholarFromYith) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message qualified as Msg

newtype ScholarFromYith = ScholarFromYith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scholarFromYith :: EnemyCard ScholarFromYith
scholarFromYith =
  enemy ScholarFromYith Cards.scholarFromYith (2, Static 2, 2) (0, 1)
    & setPrey MostCardsInHand

instance HasAbilities ScholarFromYith where
  getAbilities (ScholarFromYith a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #when You AnyEnemyAttack (be a)
      , skillTestAbility
          $ restricted a 2 (exists $ EnemyIsEngagedWith You <> ReadyEnemy) parleyAction_
      ]

instance RunMessage ScholarFromYith where
  runMessage msg e@(ScholarFromYith attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscardN iid (attrs.ability 1) 2
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) iid #intellect (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      drawCards iid (attrs.ability 2) 1
      push $ Msg.EnemyEvaded iid (toId attrs)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 2) iid
      pure e
    _ -> ScholarFromYith <$> liftRunMessage msg attrs
