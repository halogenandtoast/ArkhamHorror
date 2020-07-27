{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.MinhThiPhan where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype MinhThiPhanI = MinhThiPhanI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

minhThiPhan :: MinhThiPhanI
minhThiPhan = MinhThiPhanI $ baseAttrs
  "03002"
  "Minh Thi Phan"
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 4
    , combat = 2
    , agility = 2
    }
  [Assistant]

instance (InvestigatorRunner env) => RunMessage env MinhThiPhanI where
  runMessage msg i@(MinhThiPhanI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> MinhThiPhanI <$> runMessage msg attrs
