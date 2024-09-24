module Arkham.Enemy.Cards.BarnabasMarshTheChangeIsUponHim (
  barnabasMarshTheChangeIsUponHim,
  BarnabasMarshTheChangeIsUponHim (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype BarnabasMarshTheChangeIsUponHim = BarnabasMarshTheChangeIsUponHim EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barnabasMarshTheChangeIsUponHim :: EnemyCard BarnabasMarshTheChangeIsUponHim
barnabasMarshTheChangeIsUponHim =
  enemy BarnabasMarshTheChangeIsUponHim Cards.barnabasMarshTheChangeIsUponHim (3, Static 4, 2) (1, 2)

instance HasAbilities BarnabasMarshTheChangeIsUponHim where
  getAbilities (BarnabasMarshTheChangeIsUponHim a) =
    extend
      a
      [skillTestAbility $ restrictedAbility a 1 OnSameLocation parleyAction_]

instance RunMessage BarnabasMarshTheChangeIsUponHim where
  runMessage msg e@(BarnabasMarshTheChangeIsUponHim attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeClues attrs attrs =<< perPlayer 1
      place attrs =<< selectJust (LocationWithTitle "Marsh Refinery")
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getId
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moveTokens (attrs.ability 1) attrs iid #clue 1
      doStep 2 msg
      pure e
    DoStep 2 (PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True)) -> do
      when (attrs.token #clue == 0) $ addToVictory attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> BarnabasMarshTheChangeIsUponHim <$> liftRunMessage msg attrs
