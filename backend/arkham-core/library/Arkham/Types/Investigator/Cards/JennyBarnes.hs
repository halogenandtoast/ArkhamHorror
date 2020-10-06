{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.JennyBarnes where

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
import Lens.Micro

newtype JennyBarnes = JennyBarnes Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jennyBarnes :: JennyBarnes
jennyBarnes = JennyBarnes $ baseAttrs
  "02003"
  "Jenny Barnes"
  Rogue
  Stats
    { health = 8
    , sanity = 7
    , willpower = 3
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Drifter]

instance ActionRunner env investigator => HasActions env investigator JennyBarnes where
  getActions i window (JennyBarnes attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env JennyBarnes where
  runMessage msg i@(JennyBarnes attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId ->
      i <$ runTest investigatorId (TokenValue ElderSign investigatorResources)
    AllDrawCardAndResource | not (attrs ^. defeated || attrs ^. resigned) ->
      JennyBarnes <$> runMessage msg (attrs & resources +~ 1)
    _ -> JennyBarnes <$> runMessage msg attrs
