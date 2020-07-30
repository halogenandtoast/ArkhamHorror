{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.PrestonFairmont where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype PrestonFairmontI = PrestonFairmontI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

prestonFairmont :: PrestonFairmontI
prestonFairmont = PrestonFairmontI $ baseAttrs
  "05003"
  "Preston Fairmont"
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 1
    , combat = 1
    , agility = 1
    }
  [SilverTwilight, Socialite]

instance (InvestigatorRunner env) => RunMessage env PrestonFairmontI where
  runMessage msg i@(PrestonFairmontI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> PrestonFairmontI <$> runMessage msg attrs
