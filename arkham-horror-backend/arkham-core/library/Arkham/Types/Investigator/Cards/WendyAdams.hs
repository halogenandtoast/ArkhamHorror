{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.WendyAdams where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype WendyAdamsI = WendyAdamsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wendyAdams :: WendyAdamsI
wendyAdams = WendyAdamsI $ baseAttrs
  "01005"
  "Wendy Adams"
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 3
    , combat = 1
    , agility = 4
    }
  [Drifter]

instance (InvestigatorRunner env) => RunMessage env WendyAdamsI where
  runMessage msg i@(WendyAdamsI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> WendyAdamsI <$> runMessage msg attrs
