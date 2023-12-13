module Arkham.Game.Base where

import Arkham.Ability.Types
import Arkham.ActiveCost.Base
import Arkham.Game.Settings
import Arkham.History
import Arkham.Id
import {-# SOURCE #-} Arkham.Modifier ( Modifier )
import Arkham.Phase
import Arkham.Prelude
import Arkham.SkillTest.Base
import {-# SOURCE #-} Arkham.Target

data Game

instance Eq Game
instance Show Game

gameActiveAbilities :: Game -> [Ability]
gamePhase :: Game -> Phase
gameSettings :: Game -> Settings
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
