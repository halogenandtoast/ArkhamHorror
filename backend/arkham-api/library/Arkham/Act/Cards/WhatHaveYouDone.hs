module Arkham.Act.Cards.WhatHaveYouDone where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Message.Lifted.Choose

newtype WhatHaveYouDone = WhatHaveYouDone ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatHaveYouDone :: ActCard WhatHaveYouDone
whatHaveYouDone = act (3, A) WhatHaveYouDone Cards.whatHaveYouDone Nothing

instance HasAbilities WhatHaveYouDone where
  getAbilities (WhatHaveYouDone x) =
    [ mkAbility x 1 $ Objective $ forced $ ifEnemyDefeated Cards.ghoulPriest
    ]

instance RunMessage WhatHaveYouDone where
  runMessage msg a@(WhatHaveYouDone attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      chooseOneM lead do
        labeled "It was never much of a home. Burn it down! (→ _R1_)" $ push R1
        labeled "This \"hell-pit\" is my home! No way we are burning it! (→ _R2_)" $ push R2
      pure a
    _ -> WhatHaveYouDone <$> liftRunMessage msg attrs
