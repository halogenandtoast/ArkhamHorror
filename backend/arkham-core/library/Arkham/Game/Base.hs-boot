module Arkham.Game.Base where

import Arkham.Prelude
import Arkham.History
import Arkham.Id
import Arkham.Phase
import {-# SOURCE #-} Arkham.Target
import {-# SOURCE #-} Arkham.Modifier
import Arkham.Ability.Types
import Arkham.SkillTest.Base
import Arkham.ActiveCost.Base

data Game

instance Eq Game
instance Show Game

gameActiveAbilities :: Game -> [Ability]
gamePhase :: Game -> Phase
gameSkillTest :: Game -> Maybe SkillTest
gameActiveCost :: Game -> HashMap ActiveCostId ActiveCost
gameModifiers :: Game -> HashMap Target [Modifier]
gameWindowDepth :: Game -> Int
gameDepthLock :: Game -> Int
gamePhaseHistory :: Game -> HashMap InvestigatorId History
gameTurnHistory :: Game -> HashMap InvestigatorId History
gameRoundHistory :: Game -> HashMap InvestigatorId History

-- Time Warp
gameActionCanBeUndone :: Game -> Bool

