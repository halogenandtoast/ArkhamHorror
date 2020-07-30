{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.FinnEdwards where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype FinnEdwardsI = FinnEdwardsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

finnEdwards :: FinnEdwardsI
finnEdwards = FinnEdwardsI $ baseAttrs
  "04003"
  "Finn Edwards"
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 4
    , combat = 3
    , agility = 4
    }
  [Criminal]

instance (InvestigatorRunner env) => RunMessage env FinnEdwardsI where
  runMessage msg i@(FinnEdwardsI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> FinnEdwardsI <$> runMessage msg attrs
