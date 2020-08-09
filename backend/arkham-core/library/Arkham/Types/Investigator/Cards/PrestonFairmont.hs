{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.PrestonFairmont where

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

newtype PrestonFairmont = PrestonFairmont Attrs
  deriving newtype (Show, ToJSON, FromJSON)

prestonFairmont :: PrestonFairmont
prestonFairmont = PrestonFairmont $ baseAttrs
  "05003"
  "Preston Fairmont"
  Rogue
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 1
    , combat = 1
    , agility = 1
    }
  [SilverTwilight, Socialite]

instance (InvestigatorRunner env) => RunMessage env PrestonFairmont where
  runMessage msg i@(PrestonFairmont attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> PrestonFairmont <$> runMessage msg attrs
