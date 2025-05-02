module Arkham.Enemy.Cards.TheOrganistDrapedInMystery (theOrganistDrapedInMystery) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype TheOrganistDrapedInMystery = TheOrganistDrapedInMystery EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheOrganistDrapedInMystery where
  getModifiersFor (TheOrganistDrapedInMystery attrs) =
    modifySelf attrs [CannotBeDamaged]

instance HasAbilities TheOrganistDrapedInMystery where
  getAbilities (TheOrganistDrapedInMystery attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ PhaseEnds #when #enemy

theOrganistDrapedInMystery :: EnemyCard TheOrganistDrapedInMystery
theOrganistDrapedInMystery =
  enemyWith
    TheOrganistDrapedInMystery
    Cards.theOrganistDrapedInMystery
    (3, Static 1, 5)
    (0, 1)
    (healthL .~ Nothing)

instance RunMessage TheOrganistDrapedInMystery where
  runMessage msg e@(TheOrganistDrapedInMystery attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ investigatorEngagedWith (toId attrs)
      if null investigators
        then moveOrganistAwayFromNearestInvestigator
        else for_ investigators (`disengageEnemy` attrs)
      pure e
    _ -> TheOrganistDrapedInMystery <$> liftRunMessage msg attrs
