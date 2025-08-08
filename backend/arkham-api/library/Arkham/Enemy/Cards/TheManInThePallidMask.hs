module Arkham.Enemy.Cards.TheManInThePallidMask (theManInThePallidMask) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Modifier

newtype TheManInThePallidMask = TheManInThePallidMask EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theManInThePallidMask :: EnemyCard TheManInThePallidMask
theManInThePallidMask =
  enemy TheManInThePallidMask Cards.theManInThePallidMask (4, Static 3, 4) (0, 1)
    & setSpawnAt (FarthestLocationFromAll Anywhere)

instance HasAbilities TheManInThePallidMask where
  getAbilities (TheManInThePallidMask a) = extend1 a $ restricted a 1 OnSameLocation investigateAction_

instance RunMessage TheManInThePallidMask where
  runMessage msg e@(TheManInThePallidMask attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        sid <- getRandom
        skillTestModifier sid (attrs.ability 1) lid (ShroudModifier 2)
        investigateEdit_ sid iid (attrs.ability 1) (setTarget attrs)
      pure e
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      -- Tomb of Shadows will prevent the man in the pallid mask from being
      -- defeated, but because we have no good way of cancelling an aspect of
      -- an ability, we handle it here
      canBeDefeated <- selectNone $ locationWithEnemy attrs <> locationIs Locations.tombOfShadows
      when canBeDefeated $ defeatEnemy attrs.id iid attrs
      pure e
    _ -> TheManInThePallidMask <$> liftRunMessage msg attrs
