module Arkham.Location.Cards.WaitingRoom (
  waitingRoom,
  WaitingRoom (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Token

newtype WaitingRoom = WaitingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waitingRoom :: LocationCard WaitingRoom
waitingRoom = location WaitingRoom Cards.waitingRoom 3 (PerPlayer 1)

instance HasAbilities WaitingRoom where
  getAbilities (WaitingRoom attrs) =
    withRevealedAbilities
      attrs
      [ scenarioI18n $ withI18nTooltip "waitingRoom.resign" $ locationResignAction attrs
      | not isInfested
      ]
   where
    isInfested = countTokens #damage (locationTokens attrs) > 0

instance RunMessage WaitingRoom where
  runMessage msg (WaitingRoom attrs) =
    WaitingRoom <$> runMessage msg attrs
