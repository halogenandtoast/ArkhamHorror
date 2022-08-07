module Arkham.Act.Cards.TheGateOpens
  ( TheGateOpens(..)
  , theGateOpens
  ) where

import Arkham.Prelude

import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution

newtype TheGateOpens = TheGateOpens ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGateOpens :: ActCard TheGateOpens
theGateOpens = act
  (3, A)
  TheGateOpens
  Cards.theGateOpens
  (Just $ GroupClueCost (PerPlayer 2) (LocationWithTitle "Sentinel Peak"))

instance RunMessage TheGateOpens where
  runMessage msg a@(TheGateOpens attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    _ -> TheGateOpens <$> runMessage msg attrs
