{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.JennyBarnes where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype JennyBarnesI = JennyBarnesI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jennyBarnes :: JennyBarnesI
jennyBarnes = JennyBarnesI $ baseAttrs
  "02003"
  "Jenny Barnes"
  Stats
    { health = 8
    , sanity = 7
    , willpower = 3
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Drifter]

instance (InvestigatorRunner env) => RunMessage env JennyBarnesI where
  runMessage msg i@(JennyBarnesI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> JennyBarnesI <$> runMessage msg attrs
