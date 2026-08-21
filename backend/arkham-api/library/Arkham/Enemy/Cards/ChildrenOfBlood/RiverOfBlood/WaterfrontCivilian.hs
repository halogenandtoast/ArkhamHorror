module Arkham.Enemy.Cards.ChildrenOfBlood.RiverOfBlood.WaterfrontCivilian (waterfrontCivilian) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Enemy.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Arkham))

newtype WaterfrontCivilian = WaterfrontCivilian EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterfrontCivilian :: EnemyCard WaterfrontCivilian
waterfrontCivilian =
  enemy WaterfrontCivilian Cards.waterfrontCivilian
    & setSpawnAt (EmptyLocation <> LocationWithTrait Arkham)

instance HasAbilities WaterfrontCivilian where
  getAbilities (WaterfrontCivilian a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage WaterfrontCivilian where
  runMessage msg e@(WaterfrontCivilian attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      card <- getCard attrs.cardId
      removeFromGame attrs
      push $ SetCardAside card
      pure e
    _ -> WaterfrontCivilian <$> liftRunMessage msg attrs
