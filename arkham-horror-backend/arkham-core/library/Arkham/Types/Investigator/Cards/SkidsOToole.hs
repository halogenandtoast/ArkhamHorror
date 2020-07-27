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

newtype SkidsOTooleI = SkidsOTooleI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

skidsOToole :: SkidsOTooleI
skidsOToole = SkidsOTooleI $ baseAttrs
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

instance (InvestigatorRunner env) => RunMessage env SkidsOTooleI where
  runMessage msg i@(SkidsOTooleI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> SkidsOTooleI <$> runMessage msg attrs
