{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.SkidsOToole where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype SkidsOToole = SkidsOToole Attrs
  deriving newtype (Show, ToJSON, FromJSON)

skidsOToole :: SkidsOToole
skidsOToole = SkidsOToole $ baseAttrs
  "01003"
  "\"Skids\" O'Toole"
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 3
    , combat = 3
    , agility = 4
    }
  [Criminal]

instance (InvestigatorRunner env) => RunMessage env SkidsOToole where
  runMessage msg i@(SkidsOToole attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> SkidsOToole <$> runMessage msg attrs
