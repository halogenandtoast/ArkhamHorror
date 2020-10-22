{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.FatherMateo where

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

newtype FatherMateo = FatherMateo Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env FatherMateo where
  getModifiersFor source target (FatherMateo attrs) =
    getModifiersFor source target attrs

fatherMateo :: FatherMateo
fatherMateo = FatherMateo $ baseAttrs
  "04004"
  "Father Mateo"
  Mystic
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 3
    , combat = 2
    , agility = 3
    }
  [Believer, Warden]

instance ActionRunner env => HasActions env FatherMateo where
  getActions i window (FatherMateo attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env FatherMateo where
  runMessage msg i@(FatherMateo attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> FatherMateo <$> runMessage msg attrs
