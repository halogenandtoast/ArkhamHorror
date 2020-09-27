{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.MandyThompson where

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

newtype MandyThompson = MandyThompson Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mandyThompson :: MandyThompson
mandyThompson = MandyThompson $ baseAttrs
  "06002"
  "Mandy Thompson"
  Seeker
  Stats
    { health = 6
    , sanity = 8
    , willpower = 3
    , intellect = 5
    , combat = 1
    , agility = 3
    }
  [Assistant, Scholar]

instance HasActions env investigator MandyThompson where
  getActions i window (MandyThompson attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env MandyThompson where
  runMessage msg i@(MandyThompson attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> MandyThompson <$> runMessage msg attrs
