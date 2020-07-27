{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.MarkHarrigan where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype MarkHarriganI = MarkHarriganI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

markHarrigan :: MarkHarriganI
markHarrigan = MarkHarriganI $ baseAttrs
  "03001"
  "Mark Harrigan"
  Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 2
    , combat = 5
    , agility = 3
    }
  [Veteran]

instance (InvestigatorRunner env) => RunMessage env MarkHarriganI where
  runMessage msg i@(MarkHarriganI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> MarkHarriganI <$> runMessage msg attrs
