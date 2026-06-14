module Arkham.Act.Cards.BackThroughTheMachine (backThroughTheMachine) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Key
import Arkham.Matcher

newtype BackThroughTheMachine = BackThroughTheMachine ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backThroughTheMachine :: ActCard BackThroughTheMachine
backThroughTheMachine = act (2, A) BackThroughTheMachine Cards.backThroughTheMachine Nothing

instance HasAbilities BackThroughTheMachine where
  getAbilities (BackThroughTheMachine a) =
    [restricted a 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow]

instance RunMessage BackThroughTheMachine where
  runMessage msg a@(BackThroughTheMachine attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      push $ if headedWest then R1 else R2
      pure a
    _ -> BackThroughTheMachine <$> liftRunMessage msg attrs
