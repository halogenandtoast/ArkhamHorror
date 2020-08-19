{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.StellaClark where

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

newtype StellaClark = StellaClark Attrs
  deriving newtype (Show, ToJSON, FromJSON)

stellaClark :: StellaClark
stellaClark = StellaClark $ baseAttrs
  "60501"
  "Stella Clark"
  Survivor
  Stats
    { health = 8
    , sanity = 8
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 4
    }
  [Chosen, Civic]

instance (InvestigatorRunner Attrs env) => RunMessage env StellaClark where
  runMessage msg i@(StellaClark attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> StellaClark <$> runMessage msg attrs
