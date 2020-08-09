{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.WinifredHabbamock where

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

newtype WinifredHabbamock = WinifredHabbamock Attrs
  deriving newtype (Show, ToJSON, FromJSON)

winifredHabbamock :: WinifredHabbamock
winifredHabbamock = WinifredHabbamock $ baseAttrs
  "60301"
  "Winifred Habbamock"
  Rogue
  Stats
    { health = 8
    , sanity = 7
    , willpower = 1
    , intellect = 3
    , combat = 3
    , agility = 5
    }
  [Criminal]

instance (InvestigatorRunner env) => RunMessage env WinifredHabbamock where
  runMessage msg i@(WinifredHabbamock attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> WinifredHabbamock <$> runMessage msg attrs
