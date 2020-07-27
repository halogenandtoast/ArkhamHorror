{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.TommyMuldoon where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype TommyMuldoonI = TommyMuldoonI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

tommyMuldoon :: TommyMuldoonI
tommyMuldoon = TommyMuldoonI $ baseAttrs
  "06001"
  "Tommy Muldoon"
  Stats
    { health = 8
    , sanity = 6
    , willpower = 3
    , intellect = 3
    , combat = 4
    , agility = 2
    }
  [Police, Warden]

instance (InvestigatorRunner env) => RunMessage env TommyMuldoonI where
  runMessage msg i@(TommyMuldoonI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> TommyMuldoonI <$> runMessage msg attrs
