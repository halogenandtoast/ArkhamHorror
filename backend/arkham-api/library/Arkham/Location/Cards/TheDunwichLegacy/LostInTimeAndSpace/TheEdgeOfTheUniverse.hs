module Arkham.Location.Cards.TheDunwichLegacy.LostInTimeAndSpace.TheEdgeOfTheUniverse (theEdgeOfTheUniverse) where

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.TheDunwichLegacy.LostInTimeAndSpace qualified as Cards (
  theEdgeOfTheUniverse,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Phase

newtype TheEdgeOfTheUniverse = TheEdgeOfTheUniverse LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theEdgeOfTheUniverse :: LocationCard TheEdgeOfTheUniverse
theEdgeOfTheUniverse = location TheEdgeOfTheUniverse Cards.theEdgeOfTheUniverse 2 (PerPlayer 2)

instance HasModifiersFor TheEdgeOfTheUniverse where
  getModifiersFor (TheEdgeOfTheUniverse a) = do
    phase <- getPhase
    modifySelectWhen a (phase == UpkeepPhase) (investigatorAt a) [CannotDrawCards]
    -- "You must have at least 2 clues in order to move to The Edge of the
    -- Universe." This has to be a modifier rather than criteria on the move
    -- action, or any other move effect (Safeguard, Elusive, ...) bypasses it.
    modifySelect a (InvestigatorWithClues $ lessThan 2) [CannotEnter a.id]
