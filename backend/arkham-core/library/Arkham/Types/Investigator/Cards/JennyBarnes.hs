module Arkham.Types.Investigator.Cards.JennyBarnes where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait

newtype JennyBarnes = JennyBarnes InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

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

instance HasTokenValue env JennyBarnes where
  getTokenValue (JennyBarnes attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier $ investigatorResources attrs)
  getTokenValue (JennyBarnes attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env JennyBarnes where
  runMessage msg (JennyBarnes attrs) = case msg of
    -- TODO: Move this to a modifier
    AllDrawCardAndResource | not (attrs ^. defeatedL || attrs ^. resignedL) ->
      JennyBarnes <$> runMessage msg (attrs & resourcesL +~ 1)
    _ -> JennyBarnes <$> runMessage msg attrs
