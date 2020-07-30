{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.FatherMateo where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype FatherMateoI = FatherMateoI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fatherMateo :: FatherMateoI
fatherMateo = FatherMateoI $ baseAttrs
  "04004"
  "Father Mateo"
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 3
    , combat = 2
    , agility = 3
    }
  [Believer, Warden]

instance (InvestigatorRunner env) => RunMessage env FatherMateoI where
  runMessage msg i@(FatherMateoI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> FatherMateoI <$> runMessage msg attrs
