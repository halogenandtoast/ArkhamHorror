{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.NormanWithers where

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

newtype NormanWithers = NormanWithers Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env NormanWithers where
  getModifiersFor source target (NormanWithers attrs) =
    getModifiersFor source target attrs

normanWithers :: NormanWithers
normanWithers = NormanWithers $ baseAttrs
  "98007"
  "Norman Withers"
  Seeker
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 5
    , combat = 2
    , agility = 1
    }
  [Miskatonic]

instance ActionRunner env => HasActions env NormanWithers where
  getActions i window (NormanWithers attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env NormanWithers where
  runMessage msg i@(NormanWithers attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> NormanWithers <$> runMessage msg attrs
