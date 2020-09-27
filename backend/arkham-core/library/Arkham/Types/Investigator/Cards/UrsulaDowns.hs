{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.UrsulaDowns where

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

newtype UrsulaDowns = UrsulaDowns Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ursulaDowns :: UrsulaDowns
ursulaDowns = UrsulaDowns $ baseAttrs
  "04002"
  "Ursula Downs"
  Seeker
  Stats
    { health = 7
    , sanity = 7
    , willpower = 3
    , intellect = 4
    , combat = 1
    , agility = 4
    }
  [Wayfarer]

instance HasActions env investigator UrsulaDowns where
  getActions i window (UrsulaDowns attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env UrsulaDowns where
  runMessage msg i@(UrsulaDowns attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> UrsulaDowns <$> runMessage msg attrs
