{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AmandaSharpe where

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

newtype AmandaSharpe = AmandaSharpe Attrs
  deriving newtype (Show, ToJSON, FromJSON)

amandaSharpe :: AmandaSharpe
amandaSharpe = AmandaSharpe $ baseAttrs
  "07002"
  "Amanda Sharpe"
  Seeker
  Stats
    { health = 7
    , sanity = 7
    , willpower = 2
    , intellect = 2
    , combat = 2
    , agility = 2
    }
  [Miskatonic, Scholar]

instance HasActions env investigator AmandaSharpe where
  getActions i window (AmandaSharpe attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env AmandaSharpe where
  runMessage msg i@(AmandaSharpe attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> AmandaSharpe <$> runMessage msg attrs
