module Arkham.Enemy.Cards.GlacialPhantasm (glacialPhantasm, GlacialPhantasm (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Capability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype GlacialPhantasm = GlacialPhantasm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glacialPhantasm :: EnemyCard GlacialPhantasm
glacialPhantasm = enemy GlacialPhantasm Cards.glacialPhantasm (4, Static 4, 2) (1, 1)

instance HasAbilities GlacialPhantasm where
  getAbilities (GlacialPhantasm a) =
    extend1 a $ restricted a 1 (thisExists a ReadyEnemy) $ forced $ PhaseEnds #when #enemy

instance RunMessage GlacialPhantasm where
  runMessage msg e@(GlacialPhantasm attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      moveTowardsMatching (attrs.ability 1) attrs $ LocationWithMostInvestigators Anywhere
      eachInvestigator (`forInvestigator` msg)
      pure e
    ForInvestigator iid (UseThisAbility _ (isSource attrs -> True) 1) -> do
      whenM (iid <=~> InvestigatorAt (orConnected $ locationWithEnemy attrs)) do
        canShuffle <- can.manipulate.deck iid
        if canShuffle
          then do
            cards <- getTekelili 1
            if null cards
              then assignHorror iid (attrs.ability 1) 1
              else addTekelili iid cards
          else assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> GlacialPhantasm <$> liftRunMessage msg attrs
