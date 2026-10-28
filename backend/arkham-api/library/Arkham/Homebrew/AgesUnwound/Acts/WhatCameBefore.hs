module Arkham.Homebrew.AgesUnwound.Acts.WhatCameBefore (whatCameBefore) where

import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (ShroudModifier), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers qualified as Scenario
import Arkham.Homebrew.AgesUnwound.Traits (pattern Interior)
import Arkham.Matcher

newtype WhatCameBefore = WhatCameBefore ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatCameBefore :: ActCard WhatCameBefore
whatCameBefore = act (1, C) WhatCameBefore Cards.whatCameBefore Nothing

instance HasModifiersFor WhatCameBefore where
  getModifiersFor (WhatCameBefore a) = do
    modifySelect a (LocationWithTrait Interior) [ShroudModifier 2]
    Scenario.pastObjectiveModifier a TheBoundaryIsBroken

instance HasAbilities WhatCameBefore where
  getAbilities (WhatCameBefore a) =
    [ Scenario.pastObjective a 1 TheBoundaryIsBroken
    ]

instance RunMessage WhatCameBefore where
  runMessage msg a@(WhatCameBefore attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      frontDoor <- getHasRecord TheInvestigatorsUsedTheSchoolsFrontDoor
      advanceToAct attrs (if frontDoor then Cards.hellishHound else Cards.wardedWay) C
      pure a
    _ -> WhatCameBefore <$> liftRunMessage msg attrs
