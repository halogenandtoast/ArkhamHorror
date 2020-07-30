{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.RitaYoung where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype RitaYoungI = RitaYoungI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ritaYoung :: RitaYoungI
ritaYoung = RitaYoungI $ baseAttrs
  "05005"
  "Rita Young"
  Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 5
    }
  [Miskatonic]

instance (InvestigatorRunner env) => RunMessage env RitaYoungI where
  runMessage msg i@(RitaYoungI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> RitaYoungI <$> runMessage msg attrs
