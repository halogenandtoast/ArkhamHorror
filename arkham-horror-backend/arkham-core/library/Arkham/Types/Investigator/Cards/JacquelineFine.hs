{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.JacquelineFine where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype JacquelineFineI = JacquelineFineI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jacquelineFine :: JacquelineFineI
jacquelineFine = JacquelineFineI $ baseAttrs
  "60401"
  "Jacqueline Fine"
  Stats
    { health = 6
    , sanity = 9
    , willpower = 5
    , intellect = 3
    , combat = 2
    , agility = 2
    }
  [Clairvoyant]

instance (InvestigatorRunner env) => RunMessage env JacquelineFineI where
  runMessage msg i@(JacquelineFineI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> JacquelineFineI <$> runMessage msg attrs
