{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.LeoAnderson where

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

newtype LeoAnderson = LeoAnderson Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env LeoAnderson where
  getModifiersFor source target (LeoAnderson attrs) =
    getModifiersFor source target attrs

leoAnderson :: LeoAnderson
leoAnderson = LeoAnderson $ baseAttrs
  "04001"
  "Leo Anderson"
  Guardian
  Stats
    { health = 8
    , sanity = 6
    , willpower = 4
    , intellect = 3
    , combat = 4
    , agility = 1
    }
  [Veteran, Wayfarer]

instance ActionRunner env => HasActions env LeoAnderson where
  getActions i window (LeoAnderson attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env LeoAnderson where
  runMessage msg i@(LeoAnderson attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> LeoAnderson <$> runMessage msg attrs
