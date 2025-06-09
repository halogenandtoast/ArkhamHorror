module Arkham.Act.Cards.WhatHaveYouDone (whatHaveYouDone) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheGathering.Helpers

newtype WhatHaveYouDone = WhatHaveYouDone ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatHaveYouDone :: ActCard WhatHaveYouDone
whatHaveYouDone = act (3, A) WhatHaveYouDone Cards.whatHaveYouDone Nothing

instance HasAbilities WhatHaveYouDone where
  getAbilities (WhatHaveYouDone x) =
    [mkAbility x 1 $ Objective $ forced $ ifEnemyDefeated Cards.ghoulPriest]

instance RunMessage WhatHaveYouDone where
  runMessage msg a@(WhatHaveYouDone attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      selectEach (InPlayEnemy $ enemyIs Cards.ghoulPriest) addToVictory
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      leadChooseOneM do
        labeledI18n "r1" $ push R1
        labeledI18n "r2" $ push R2
      pure a
    _ -> WhatHaveYouDone <$> liftRunMessage msg attrs
