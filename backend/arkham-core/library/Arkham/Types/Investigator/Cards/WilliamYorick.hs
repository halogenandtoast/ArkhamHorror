module Arkham.Types.Investigator.Cards.WilliamYorick where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait

newtype WilliamYorick = WilliamYorick InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env WilliamYorick where
  getModifiersFor source target (WilliamYorick attrs) =
    getModifiersFor source target attrs

williamYorick :: WilliamYorick
williamYorick = WilliamYorick $ baseAttrs
  "03005"
  "William Yorick"
  Survivor
  Stats
    { health = 8
    , sanity = 6
    , willpower = 3
    , intellect = 2
    , combat = 4
    , agility = 3
    }
  [Warden]

instance HasTokenValue env WilliamYorick where
  getTokenValue (WilliamYorick attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (WilliamYorick attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => HasActions env WilliamYorick where
  getActions i window (WilliamYorick attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env WilliamYorick where
  runMessage msg (WilliamYorick attrs) = WilliamYorick <$> runMessage msg attrs
