{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.RitaYoung where

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

newtype RitaYoung = RitaYoung Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env RitaYoung where
  getModifiersFor source target (RitaYoung attrs) =
    getModifiersFor source target attrs

ritaYoung :: RitaYoung
ritaYoung = RitaYoung $ baseAttrs
  "05005"
  "Rita Young"
  Survivor
  Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 5
    }
  [Miskatonic]

instance ActionRunner env => HasActions env RitaYoung where
  getActions i window (RitaYoung attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env RitaYoung where
  runMessage msg i@(RitaYoung attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> RitaYoung <$> runMessage msg attrs
