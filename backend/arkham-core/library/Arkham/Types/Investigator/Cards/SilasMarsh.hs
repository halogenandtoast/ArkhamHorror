{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.SilasMarsh where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype SilasMarsh = SilasMarsh Attrs
  deriving newtype (Show, ToJSON, FromJSON)

silasMarsh :: SilasMarsh
silasMarsh = SilasMarsh $ baseAttrs
  "98013"
  "Silas Marsh"
  Stats
    { health = 9
    , sanity = 5
    , willpower = 2
    , intellect = 2
    , combat = 4
    , agility = 4
    }
  [Drifter]

instance (InvestigatorRunner env) => RunMessage env SilasMarsh where
  runMessage msg i@(SilasMarsh attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> SilasMarsh <$> runMessage msg attrs
