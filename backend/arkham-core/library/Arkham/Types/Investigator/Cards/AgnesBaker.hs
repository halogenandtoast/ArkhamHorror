{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AgnesBaker where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype AgnesBakerI = AgnesBakerI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

agnesBaker :: AgnesBakerI
agnesBaker = AgnesBakerI $ baseAttrs
  "01004"
  "Agnes Baker"
  Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 2
    , agility = 3
    }
  [Sorcerer]

instance (InvestigatorRunner env) => RunMessage env AgnesBakerI where
  runMessage msg i@(AgnesBakerI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> AgnesBakerI <$> runMessage msg attrs
