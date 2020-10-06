{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.FinnEdwards where

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

newtype FinnEdwards = FinnEdwards Attrs
  deriving newtype (Show, ToJSON, FromJSON)

finnEdwards :: FinnEdwards
finnEdwards = FinnEdwards $ baseAttrs
  "04003"
  "Finn Edwards"
  Rogue
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 4
    , combat = 3
    , agility = 4
    }
  [Criminal]

instance ActionRunner env investigator => HasActions env investigator FinnEdwards where
  getActions i window (FinnEdwards attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env FinnEdwards where
  runMessage msg i@(FinnEdwards attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> FinnEdwards <$> runMessage msg attrs
