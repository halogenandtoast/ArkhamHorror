{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.StellaClark where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype StellaClarkI = StellaClarkI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

stellaClark :: StellaClarkI
stellaClark = StellaClarkI $ baseAttrs
  "60501"
  "Stella Clark"
  Stats
    { health = 8
    , sanity = 8
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 4
    }
  [Chosen, Civic]

instance (InvestigatorRunner env) => RunMessage env StellaClarkI where
  runMessage msg i@(StellaClarkI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> StellaClarkI <$> runMessage msg attrs
