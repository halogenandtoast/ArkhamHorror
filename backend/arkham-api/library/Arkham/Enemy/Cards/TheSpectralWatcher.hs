module Arkham.Enemy.Cards.TheSpectralWatcher (theSpectralWatcher) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Enemy (disengageEnemyFromAll)
import Arkham.Matcher

newtype TheSpectralWatcher = TheSpectralWatcher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSpectralWatcher :: EnemyCard TheSpectralWatcher
theSpectralWatcher = enemy TheSpectralWatcher Cards.theSpectralWatcher (3, Static 5, 3) (1, 1)

instance HasAbilities TheSpectralWatcher where
  getAbilities (TheSpectralWatcher a) =
    extend1 a
      $ groupLimit PerTestOrAbility
      $ mkAbility a 1
      $ forced
      $ EnemyDefeated #when Anyone ByAny (be a)

instance RunMessage TheSpectralWatcher where
  runMessage msg e@(TheSpectralWatcher attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- NOTE: This is supposed to be instead of discard, but the window is when they are defeated
      -- So we really need to cancel the instead of defeat, BUT still trigger all the associated windows
      insteadOfDefeatWithWindows attrs $ do
        healAllDamage (attrs.ability 1) attrs
        disengageEnemyFromAll attrs
        exhaustThis attrs
        doesNotReadyDuringUpkeep (attrs.ability 1) attrs
      pure e
    _ -> TheSpectralWatcher <$> liftRunMessage msg attrs
