{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.TonyMorgan where

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

newtype TonyMorgan = TonyMorgan Attrs
  deriving newtype (Show, ToJSON, FromJSON)

tonyMorgan :: TonyMorgan
tonyMorgan = TonyMorgan $ baseAttrs
  "06003"
  "Tony Morgan"
  Rogue
  Stats
    { health = 9
    , sanity = 5
    , willpower = 2
    , intellect = 3
    , combat = 5
    , agility = 2
    }
  [Criminal, Hunter]

instance ActionRunner env => HasActions env TonyMorgan where
  getActions i window (TonyMorgan attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env TonyMorgan where
  runMessage msg i@(TonyMorgan attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> TonyMorgan <$> runMessage msg attrs
