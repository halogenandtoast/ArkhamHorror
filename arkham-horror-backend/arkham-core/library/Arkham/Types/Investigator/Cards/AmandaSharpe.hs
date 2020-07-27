{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AmandaSharpe where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype AmandaSharpeI = AmandaSharpeI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

amandaSharpe :: AmandaSharpeI
amandaSharpe = AmandaSharpeI $ baseAttrs
  "07002"
  "Amanda Sharpe"
  Stats
    { health = 7
    , sanity = 7
    , willpower = 2
    , intellect = 2
    , combat = 2
    , agility = 2
    }
  [Miskatonic, Scholar]

instance (InvestigatorRunner env) => RunMessage env AmandaSharpeI where
  runMessage msg i@(AmandaSharpeI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> AmandaSharpeI <$> runMessage msg attrs
