{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.WendyAdams where

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

newtype WendyAdams = WendyAdams Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wendyAdams :: WendyAdams
wendyAdams = WendyAdams $ baseAttrs
  "01005"
  "Wendy Adams"
  Survivor
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 3
    , combat = 1
    , agility = 4
    }
  [Drifter]

instance (InvestigatorRunner env) => RunMessage env WendyAdams where
  runMessage msg i@(WendyAdams attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> WendyAdams <$> runMessage msg attrs
