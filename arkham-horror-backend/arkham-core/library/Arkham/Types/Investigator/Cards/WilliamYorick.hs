{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.WilliamYorick where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype WilliamYorickI = WilliamYorickI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

williamYorick :: WilliamYorickI
williamYorick = WilliamYorickI $ baseAttrs
  "03005"
  "William Yorick"
  Stats
    { health = 8
    , sanity = 6
    , willpower = 3
    , intellect = 2
    , combat = 4
    , agility = 3
    }
  [Warden]

instance (InvestigatorRunner env) => RunMessage env WilliamYorickI where
  runMessage msg i@(WilliamYorickI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> WilliamYorickI <$> runMessage msg attrs
