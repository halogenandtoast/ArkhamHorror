{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.SilasMarsh where

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

newtype SilasMarsh = SilasMarsh Attrs
  deriving newtype (Show, ToJSON, FromJSON)

silasMarsh :: SilasMarsh
silasMarsh = SilasMarsh $ baseAttrs
  "98013"
  "Silas Marsh"
  Survivor
  Stats
    { health = 9
    , sanity = 5
    , willpower = 2
    , intellect = 2
    , combat = 4
    , agility = 4
    }
  [Drifter]

instance ActionRunner env => HasActions env SilasMarsh where
  getActions i window (SilasMarsh attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env SilasMarsh where
  runMessage msg i@(SilasMarsh attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> SilasMarsh <$> runMessage msg attrs
