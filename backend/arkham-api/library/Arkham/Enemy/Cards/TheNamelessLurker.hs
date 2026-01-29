module Arkham.Enemy.Cards.TheNamelessLurker (theNamelessLurker) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype TheNamelessLurker = TheNamelessLurker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNamelessLurker :: EnemyCard TheNamelessLurker
theNamelessLurker =
  enemy TheNamelessLurker Cards.theNamelessLurker (2, Static 1, 2) (0, 2)
    & setSpawnAt (FarthestLocationFromInvestigator You EmptyLocation)

instance HasAbilities TheNamelessLurker where
  getAbilities (TheNamelessLurker a) =
    extend1 a
      $ restricted a 1 (thisExists a $ ReadyEnemy <> EnemyWithoutDoom)
      $ forced
      $ PhaseEnds #when #investigation

instance RunMessage TheNamelessLurker where
  runMessage msg e@(TheNamelessLurker attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> TheNamelessLurker <$> liftRunMessage msg attrs
