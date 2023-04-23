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
gameActiveCost :: Game -> Map ActiveCostId ActiveCost
gameModifiers :: Game -> Map Target [Modifier]
gameWindowDepth :: Game -> Int
gameDepthLock :: Game -> Int
gamePhaseHistory :: Game -> Map InvestigatorId History
gameTurnHistory :: Game -> Map InvestigatorId History
gameRoundHistory :: Game -> Map InvestigatorId History
gameIgnoreCanModifiers :: Game -> Bool

-- Time Warp
gameActionCanBeUndone :: Game -> Bool

