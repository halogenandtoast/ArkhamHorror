{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.RexMurphy where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype RexMurphyI = RexMurphyI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rexMurphy :: RexMurphyI
rexMurphy = RexMurphyI $ baseAttrs
  "02002"
  "Rex Murphy"
  Stats
    { health = 6
    , sanity = 9
    , willpower = 3
    , intellect = 4
    , combat = 2
    , agility = 3
    }
  [Reporter]

instance (InvestigatorRunner env) => RunMessage env RexMurphyI where
  runMessage msg i@(RexMurphyI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> RexMurphyI <$> runMessage msg attrs
