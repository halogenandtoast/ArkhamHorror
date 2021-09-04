module Arkham.Types.Scenario.Scenarios.CurtainCall
  ( CurtainCall(..)
  , curtainCall
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.InvestigatorId
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token

newtype CurtainCall = CurtainCall ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curtainCall :: Difficulty -> CurtainCall
curtainCall difficulty =
  CurtainCall
    $ baseAttrs
        "03043"
        "Curtain Call"
        ["03044", "03045"]
        ["03046", "03047a", "03047b", "03047c", "03048"]
        difficulty
    & locationLayoutL
    ?~ [ "lobbyDoorway1 .     balcony .         backstageDoorway1"
       , "lobbyDoorway3 lobby theatre backstage backstageDoorway3"
       , "lobbyDoorway2 .     .       .         backstageDoorway2"
       ]

instance HasRecord CurtainCall where
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance
  ( HasTokenValue env InvestigatorId
  , HasCount HorrorCount env InvestigatorId
  )
  => HasTokenValue env CurtainCall where
  getTokenValue (CurtainCall attrs) iid = \case
    Skull -> do
      horrorCount <- unHorrorCount <$> getCount iid
      let easyStandardModifier = if horrorCount >= 3 then 3 else 1
      let hardExpertModifier = max 1 horrorCount
      pure $ toTokenValue attrs Skull easyStandardModifier hardExpertModifier
    face | face `elem` [Cultist, Tablet, ElderThing] ->
      pure $ toTokenValue attrs face 4 5
    otherFace -> getTokenValue attrs iid otherFace

instance ScenarioRunner env => RunMessage env CurtainCall where
  runMessage msg (CurtainCall attrs) = CurtainCall <$> runMessage msg attrs
