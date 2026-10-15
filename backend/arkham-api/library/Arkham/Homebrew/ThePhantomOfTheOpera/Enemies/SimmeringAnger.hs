module Arkham.Homebrew.ThePhantomOfTheOpera.Enemies.SimmeringAnger (simmeringAnger) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (ExposeRoute (..), afterExposedViaFor)
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype SimmeringAnger = SimmeringAnger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

simmeringAnger :: EnemyCard SimmeringAnger
simmeringAnger = enemy SimmeringAnger Cards.simmeringAnger

instance HasAbilities SimmeringAnger where
  getAbilities (SimmeringAnger a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ afterExposedViaFor (Just You) ExposedByFight a

instance RunMessage SimmeringAnger where
  runMessage msg e@(SimmeringAnger attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> SimmeringAnger <$> liftRunMessage msg attrs
