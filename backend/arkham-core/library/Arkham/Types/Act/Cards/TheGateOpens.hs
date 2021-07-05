module Arkham.Types.Act.Cards.TheGateOpens
  ( TheGateOpens(..)
  , theGateOpens
  ) where

import Arkham.Prelude

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype TheGateOpens = TheGateOpens ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

theGateOpens :: TheGateOpens
theGateOpens = TheGateOpens $ baseAttrs
  "02281"
  "The Gate Opens"
  (Act 3 A)
  (Just $ RequiredClues (PerPlayer 2) (Just $ LocationWithTitle "Sentinel Peak")
  )

instance ActionRunner env => HasActions env TheGateOpens where
  getActions i window (TheGateOpens x) = getActions i window x

instance ActRunner env => RunMessage env TheGateOpens where
  runMessage msg a@(TheGateOpens attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ unshiftMessage (ScenarioResolution $ Resolution 2)
    _ -> TheGateOpens <$> runMessage msg attrs
