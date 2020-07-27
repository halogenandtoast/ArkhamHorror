{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.JimCulver where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype JimCulverI = JimCulverI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jimCulver :: JimCulverI
jimCulver = JimCulverI $ baseAttrs
  "02004"
  "Jim Culver"
  Stats
    { health = 7
    , sanity = 8
    , willpower = 4
    , intellect = 3
    , combat = 3
    , agility = 2
    }
  [Performer]

instance (InvestigatorRunner env) => RunMessage env JimCulverI where
  runMessage msg i@(JimCulverI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> JimCulverI <$> runMessage msg attrs
