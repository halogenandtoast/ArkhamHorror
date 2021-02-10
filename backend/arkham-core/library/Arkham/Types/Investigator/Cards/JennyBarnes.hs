module Arkham.Types.Investigator.Cards.JennyBarnes where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Message
import Arkham.Types.Token
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype JennyBarnes = JennyBarnes InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env JennyBarnes where
  getModifiersFor source target (JennyBarnes attrs) =
    getModifiersFor source target attrs

jennyBarnes :: JennyBarnes
jennyBarnes = JennyBarnes $ baseAttrs
  "02003"
  "Jenny Barnes"
  Rogue
  Stats
    { health = 8
    , sanity = 7
    , willpower = 3
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Drifter]

instance ActionRunner env => HasActions env JennyBarnes where
  getActions i window (JennyBarnes attrs) = getActions i window attrs

instance HasTokenValue env JennyBarnes where
  getTokenValue (JennyBarnes attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier $ investigatorResources attrs)
  getTokenValue (JennyBarnes attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env JennyBarnes where
  runMessage msg (JennyBarnes attrs) = case msg of
    AllDrawCardAndResource | not (attrs ^. defeatedL || attrs ^. resignedL) ->
      JennyBarnes <$> runMessage msg (attrs & resourcesL +~ 1)
    _ -> JennyBarnes <$> runMessage msg attrs
