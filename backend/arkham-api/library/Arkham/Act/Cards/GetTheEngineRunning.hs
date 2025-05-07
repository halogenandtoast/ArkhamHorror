module Arkham.Act.Cards.GetTheEngineRunning (getTheEngineRunning) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher

newtype GetTheEngineRunning = GetTheEngineRunning ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getTheEngineRunning :: ActCard GetTheEngineRunning
getTheEngineRunning = act (2, A) GetTheEngineRunning Cards.getTheEngineRunning Nothing

instance HasAbilities GetTheEngineRunning where
  getAbilities = actAbilities \x ->
    [ restricted x 1 (exists $ LocationWithTitle "Engine Car" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage GetTheEngineRunning where
  runMessage msg a@(GetTheEngineRunning attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    _ -> GetTheEngineRunning <$> liftRunMessage msg attrs
