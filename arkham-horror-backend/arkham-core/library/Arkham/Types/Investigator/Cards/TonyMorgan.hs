{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.TonyMorgan where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype TonyMorganI = TonyMorganI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

tonyMorgan :: TonyMorganI
tonyMorgan = TonyMorganI $ baseAttrs
  "06003"
  "Tony Morgan"
  Stats
    { health = 9
    , sanity = 5
    , willpower = 2
    , intellect = 3
    , combat = 5
    , agility = 2
    }
  [Criminal, Hunter]

instance (InvestigatorRunner env) => RunMessage env TonyMorganI where
  runMessage msg i@(TonyMorganI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> TonyMorganI <$> runMessage msg attrs
