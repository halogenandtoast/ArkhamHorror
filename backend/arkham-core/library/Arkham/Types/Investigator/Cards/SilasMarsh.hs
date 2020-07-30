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

newtype SilasMarshI = SilasMarshI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

silasMarsh :: SilasMarshI
silasMarsh = SilasMarshI $ baseAttrs
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

instance (InvestigatorRunner env) => RunMessage env SilasMarshI where
  runMessage msg i@(SilasMarshI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> SilasMarshI <$> runMessage msg attrs
