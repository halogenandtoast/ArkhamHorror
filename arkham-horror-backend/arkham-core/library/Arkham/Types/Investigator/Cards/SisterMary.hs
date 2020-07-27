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

newtype SisterMaryI = SisterMaryI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

sisterMary :: SisterMaryI
sisterMary = SisterMaryI $ baseAttrs
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

instance (InvestigatorRunner env) => RunMessage env SisterMaryI where
  runMessage msg i@(SisterMaryI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> SisterMaryI <$> runMessage msg attrs
