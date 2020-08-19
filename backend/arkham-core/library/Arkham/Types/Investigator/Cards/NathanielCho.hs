{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.NathanielCho where

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

newtype NathanielCho = NathanielCho Attrs
  deriving newtype (Show, ToJSON, FromJSON)

nathanielCho :: NathanielCho
nathanielCho = NathanielCho $ baseAttrs
  "60101"
  "Nathaniel Cho"
  Guardian
  Stats
    { health = 9
    , sanity = 6
    , willpower = 3
    , intellect = 2
    , combat = 5
    , agility = 2
    }
  [Criminal, Warden]

instance (InvestigatorRunner Attrs env) => RunMessage env NathanielCho where
  runMessage msg i@(NathanielCho attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> NathanielCho <$> runMessage msg attrs
