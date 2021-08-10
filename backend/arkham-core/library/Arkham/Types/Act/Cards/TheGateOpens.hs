module Arkham.Types.Act.Cards.TheGateOpens
  ( TheGateOpens(..)
  , theGateOpens
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype TheGateOpens = TheGateOpens ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

theGateOpens :: ActCard TheGateOpens
theGateOpens = act
  (3, A)
  TheGateOpens
  Cards.theGateOpens
  (Just $ GroupClueCost (PerPlayer 2) (Just $ LocationWithTitle "Sentinel Peak")
  )

instance ActRunner env => RunMessage env TheGateOpens where
  runMessage msg a@(TheGateOpens attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    _ -> TheGateOpens <$> runMessage msg attrs
