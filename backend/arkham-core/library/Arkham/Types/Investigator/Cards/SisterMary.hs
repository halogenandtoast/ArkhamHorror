{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.SisterMary where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype SisterMary = SisterMary Attrs
  deriving newtype (Show, ToJSON, FromJSON)

sisterMary :: SisterMary
sisterMary = SisterMary $ baseAttrs
  "07001"
  "Sister Mary"
  Stats
    { health = 5
    , sanity = 9
    , willpower = 4
    , intellect = 2
    , combat = 3
    , agility = 3
    }
  [Believer, Blessed]

instance (InvestigatorRunner env) => RunMessage env SisterMary where
  runMessage msg i@(SisterMary attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> SisterMary <$> runMessage msg attrs
