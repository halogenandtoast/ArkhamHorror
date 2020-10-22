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

instance HasModifiersFor env JacquelineFine where
  getModifiersFor source target (JacquelineFine attrs) =
    getModifiersFor source target attrs

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

instance ActionRunner env => HasActions env JacquelineFine where
  getActions i window (JacquelineFine attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env JacquelineFine where
  runMessage msg i@(JacquelineFine attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> JacquelineFine <$> runMessage msg attrs
