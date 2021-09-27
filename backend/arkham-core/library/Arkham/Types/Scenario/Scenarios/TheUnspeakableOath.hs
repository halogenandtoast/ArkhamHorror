module Arkham.Types.Scenario.Scenarios.TheUnspeakableOath
  ( TheUnspeakableOath(..)
  , theUnspeakableOath
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Acts
import qualified Arkham.Agenda.Cards as Agendas
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.InvestigatorId
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token

newtype TheUnspeakableOath = TheUnspeakableOath ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnspeakableOath :: Difficulty -> TheUnspeakableOath
theUnspeakableOath difficulty = TheUnspeakableOath $ baseAttrs
  "03159"
  "The Unspeakable Oath"
  [Agendas.lockedInside, Agendas.torturousDescent, Agendas.hisDomain]
  [ Acts.arkhamAsylum
  , Acts.theReallyBadOnesV1
  , Acts.theReallyBadOnesV2
  , Acts.planningTheEscape
  , Acts.noAsylum
  ]
  difficulty

instance HasRecord TheUnspeakableOath where
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance HasTokenValue env InvestigatorId => HasTokenValue env TheUnspeakableOath where
  getTokenValue (TheUnspeakableOath attrs) iid = \case
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue attrs iid otherFace

instance ScenarioRunner env => RunMessage env TheUnspeakableOath where
  runMessage msg (TheUnspeakableOath attrs) =
    TheUnspeakableOath <$> runMessage msg attrs
