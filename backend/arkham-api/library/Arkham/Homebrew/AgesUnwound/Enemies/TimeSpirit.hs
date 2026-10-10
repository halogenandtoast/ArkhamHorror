module Arkham.Homebrew.AgesUnwound.Enemies.TimeSpirit (timeSpirit) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype TimeSpirit = TimeSpirit EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeSpirit :: EnemyCard TimeSpirit
timeSpirit = enemy TimeSpirit Cards.timeSpirit

{- | "Forced - After you defeat Time Spirit: Gain an action."

@You@ is in the window on purpose: the action goes to whoever defeated it, and a
'Who' that does not name the performer offers the Forced trigger to every seat
(see @project_forced_window_who_must_be_you@).
-}
instance HasAbilities TimeSpirit where
  getAbilities (TimeSpirit a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #after You ByAny (be a)

instance RunMessage TimeSpirit where
  runMessage msg e@(TimeSpirit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainActions iid (attrs.ability 1) 1
      pure e
    _ -> TimeSpirit <$> liftRunMessage msg attrs
