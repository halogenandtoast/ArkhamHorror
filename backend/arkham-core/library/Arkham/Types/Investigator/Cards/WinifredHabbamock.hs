{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.WinifredHabbamock where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype WinifredHabbamockI = WinifredHabbamockI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

winifredHabbamock :: WinifredHabbamockI
winifredHabbamock = WinifredHabbamockI $ baseAttrs
  "60301"
  "Winifred Habbamock"
  Stats
    { health = 8
    , sanity = 7
    , willpower = 1
    , intellect = 3
    , combat = 3
    , agility = 5
    }
  [Criminal]

instance (InvestigatorRunner env) => RunMessage env WinifredHabbamockI where
  runMessage msg i@(WinifredHabbamockI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> WinifredHabbamockI <$> runMessage msg attrs
