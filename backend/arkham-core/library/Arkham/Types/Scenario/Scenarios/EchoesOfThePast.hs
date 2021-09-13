module Arkham.Types.Scenario.Scenarios.EchoesOfThePast
  ( EchoesOfThePast(..)
  , echoesOfThePast
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Acts
import qualified Arkham.Agenda.Cards as Agendas
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token

newtype EchoesOfThePast = EchoesOfThePast ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoesOfThePast :: Difficulty -> EchoesOfThePast
echoesOfThePast difficulty = EchoesOfThePast $ baseAttrs
  "03120"
  "Echoes of the Past"
  [ Agendas.theTruthIsHidden
  , Agendas.ransackingTheManor
  , Agendas.secretsBetterLeftHidden
  ]
  [Acts.raceForAnswers, Acts.mistakesOfThePast, Acts.theOath]
  difficulty

instance HasRecord EchoesOfThePast where
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance
  ( Query EnemyMatcher env
  , HasCount DoomCount env EnemyId
  , HasTokenValue env InvestigatorId
  )
  => HasTokenValue env EchoesOfThePast where
  getTokenValue (EchoesOfThePast attrs) iid = \case
    Skull -> do
      enemies <- selectList AnyEnemy
      doomCounts <- traverse (fmap unDoomCount . getCount) enemies
      pure $ toTokenValue
        attrs
        Skull
        (maybe 0 maximum $ fromNullable doomCounts)
        (sum doomCounts)
    Cultist -> pure $ toTokenValue attrs Cultist 2 4
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 2 4
    otherFace -> getTokenValue attrs iid otherFace

instance ScenarioRunner env => RunMessage env EchoesOfThePast where
  runMessage msg (EchoesOfThePast attrs) =
    EchoesOfThePast <$> runMessage msg attrs
