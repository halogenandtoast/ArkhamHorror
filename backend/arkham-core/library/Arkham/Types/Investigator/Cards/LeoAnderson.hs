{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.LeoAnderson where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype LeoAndersonI = LeoAndersonI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

leoAnderson :: LeoAndersonI
leoAnderson = LeoAndersonI $ baseAttrs
  "04001"
  "Leo Anderson"
  Stats
    { health = 8
    , sanity = 6
    , willpower = 4
    , intellect = 3
    , combat = 4
    , agility = 1
    }
  [Veteran, Wayfarer]

instance (InvestigatorRunner env) => RunMessage env LeoAndersonI where
  runMessage msg i@(LeoAndersonI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> LeoAndersonI <$> runMessage msg attrs
