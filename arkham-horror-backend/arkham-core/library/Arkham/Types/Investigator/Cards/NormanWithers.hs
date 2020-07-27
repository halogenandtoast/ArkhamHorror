{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.NormanWithers where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype NormanWithersI = NormanWithersI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

normanWithers :: NormanWithersI
normanWithers = NormanWithersI $ baseAttrs
  "98007"
  "Norman Withers"
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 5
    , combat = 2
    , agility = 1
    }
  [Miskatonic]

instance (InvestigatorRunner env) => RunMessage env NormanWithersI where
  runMessage msg i@(NormanWithersI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> NormanWithersI <$> runMessage msg attrs
