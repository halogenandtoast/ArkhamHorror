{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.MandyThompson where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype MandyThompsonI = MandyThompsonI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mandyThompson :: MandyThompsonI
mandyThompson = MandyThompsonI $ baseAttrs
  "06002"
  "Mandy Thompson"
  Stats
    { health = 6
    , sanity = 8
    , willpower = 3
    , intellect = 5
    , combat = 1
    , agility = 3
    }
  [Assistant, Scholar]

instance (InvestigatorRunner env) => RunMessage env MandyThompsonI where
  runMessage msg i@(MandyThompsonI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> MandyThompsonI <$> runMessage msg attrs
