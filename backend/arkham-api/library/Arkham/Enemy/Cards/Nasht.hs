module Arkham.Enemy.Cards.Nasht (nasht) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Story
import Arkham.Story.Cards qualified as Story

newtype Nasht = Nasht EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nasht :: EnemyCard Nasht
nasht = enemy Nasht Cards.nasht (2, Static 3, 2) (0, 1)

instance HasAbilities Nasht where
  getAbilities (Nasht x) =
    withBaseAbilities
      x
      [ skillTestAbility $ mkAbility x 1 parleyAction_
      , mkAbility x 2 $ forced $ EnemyDefeated #after You ByAny $ be x
      ]

instance RunMessage Nasht where
  runMessage msg e@(Nasht attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \sType ->
          skillLabeled sType $ parley sid iid (attrs.ability 1) iid sType (Fixed $ 2 + n)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      iids <- select $ InvestigatorAt $ locationWithEnemy attrs.id
      chooseOneAtATimeM iid do
        targets iids $ initiateEnemyAttack attrs (attrs.ability 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      theTrialOfNasht <- genCard Story.theTrialOfNasht
      removeEnemy attrs
      resolveStory iid theTrialOfNasht
      pure e
    _ -> Nasht <$> liftRunMessage msg attrs
