{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.DexterDrake where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype DexterDrakeI = DexterDrakeI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dexterDrake :: DexterDrakeI
dexterDrake = DexterDrakeI $ baseAttrs
  "98016"
  "Dexter Drake"
  Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 3
    , agility = 2
    }
  [Sorcerer, Veteran]

instance (InvestigatorRunner env) => RunMessage env DexterDrakeI where
  runMessage msg i@(DexterDrakeI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> DexterDrakeI <$> runMessage msg attrs
