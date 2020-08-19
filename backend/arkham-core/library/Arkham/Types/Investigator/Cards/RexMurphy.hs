{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.RexMurphy where

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

newtype RexMurphy = RexMurphy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rexMurphy :: RexMurphy
rexMurphy = RexMurphy $ baseAttrs
  "02002"
  "Rex Murphy"
  Seeker
  Stats
    { health = 6
    , sanity = 9
    , willpower = 3
    , intellect = 4
    , combat = 2
    , agility = 3
    }
  [Reporter]

instance HasActions env investigator RexMurphy where
  getActions i window (RexMurphy attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env RexMurphy where
  runMessage msg i@(RexMurphy attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> RexMurphy <$> runMessage msg attrs
