{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.CarolynFern where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype CarolynFernI = CarolynFernI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

carolynFern :: CarolynFernI
carolynFern = CarolynFernI $ baseAttrs
  "05001"
  "Carolyn Fern"
  Stats
    { health = 6
    , sanity = 9
    , willpower = 3
    , intellect = 4
    , combat = 2
    , agility = 2
    }
  [Medic]

instance (InvestigatorRunner env) => RunMessage env CarolynFernI where
  runMessage msg i@(CarolynFernI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> CarolynFernI <$> runMessage msg attrs
