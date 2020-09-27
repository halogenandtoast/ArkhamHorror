{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.CalvinWright where

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

newtype CalvinWright = CalvinWright Attrs
  deriving newtype (Show, ToJSON, FromJSON)

calvinWright :: CalvinWright
calvinWright = CalvinWright $ baseAttrs
  "04005"
  "Calvin Wright"
  Survivor
  Stats
    { health = 6
    , sanity = 6
    , willpower = 0
    , intellect = 0
    , combat = 0
    , agility = 0
    }
  [Cursed, Drifter]

instance HasActions env investigator CalvinWright where
  getActions i window (CalvinWright attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env CalvinWright where
  runMessage msg i@(CalvinWright attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> CalvinWright <$> runMessage msg attrs
