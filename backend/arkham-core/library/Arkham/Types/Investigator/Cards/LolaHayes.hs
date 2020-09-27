{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.LolaHayes where

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

newtype LolaHayes = LolaHayes Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lolaHayes :: LolaHayes
lolaHayes = LolaHayes $ baseAttrs
  "03006"
  "Lola Hayes"
  Neutral
  Stats
    { health = 6
    , sanity = 6
    , willpower = 3
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Performer]

instance HasActions env investigator LolaHayes where
  getActions i window (LolaHayes attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env LolaHayes where
  runMessage msg i@(LolaHayes attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> LolaHayes <$> runMessage msg attrs
