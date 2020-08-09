{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.JacquelineFine where

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

newtype JacquelineFine = JacquelineFine Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jacquelineFine :: JacquelineFine
jacquelineFine = JacquelineFine $ baseAttrs
  "60401"
  "Jacqueline Fine"
  Mystic
  Stats
    { health = 6
    , sanity = 9
    , willpower = 5
    , intellect = 3
    , combat = 2
    , agility = 2
    }
  [Clairvoyant]

instance (InvestigatorRunner env) => RunMessage env JacquelineFine where
  runMessage msg i@(JacquelineFine attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> JacquelineFine <$> runMessage msg attrs
