{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.MarkHarrigan where

import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype MarkHarrigan = MarkHarrigan Attrs
  deriving newtype (Show, ToJSON, FromJSON)

markHarrigan :: MarkHarrigan
markHarrigan = MarkHarrigan $ baseAttrs
  "03001"
  "Mark Harrigan"
  Guardian
  Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 2
    , combat = 5
    , agility = 3
    }
  [Veteran]

instance (InvestigatorRunner env) => RunMessage env MarkHarrigan where
  runMessage msg i@(MarkHarrigan attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> MarkHarrigan <$> runMessage msg attrs
