{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.CalvinWright where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype CalvinWrightI = CalvinWrightI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

calvinWright :: CalvinWrightI
calvinWright = CalvinWrightI $ baseAttrs
  "04005"
  "Calvin Wright"
  Stats
    { health = 6
    , sanity = 6
    , willpower = 0
    , intellect = 0
    , combat = 0
    , agility = 0
    }
  [Cursed, Drifter]

instance (InvestigatorRunner env) => RunMessage env CalvinWrightI where
  runMessage msg i@(CalvinWrightI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> CalvinWrightI <$> runMessage msg attrs
