module Arkham.Act.Cards.ExposeTheAnomaly (exposeTheAnomaly) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Oozified))

newtype ExposeTheAnomaly = ExposeTheAnomaly ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeTheAnomaly :: ActCard ExposeTheAnomaly
exposeTheAnomaly = act (1, A) ExposeTheAnomaly Cards.exposeTheAnomaly Nothing

instance HasAbilities ExposeTheAnomaly where
  getAbilities (ExposeTheAnomaly a) =
    [mkAbility a 1 $ Objective $ triggered (RoundBegins #when) $ GroupClueCost (PerPlayer 2) Anywhere]

instance RunMessage ExposeTheAnomaly where
  runMessage msg a@(ExposeTheAnomaly attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      locations <- select $ LocationWithTrait Oozified
      leadChooseOneM $ targets locations $ createEnemyAt_ Enemies.vulnerableHeart
      advanceActDeck attrs
      pure a
    _ -> ExposeTheAnomaly <$> liftRunMessage msg attrs
