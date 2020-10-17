{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.MinhThiPhan where

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

newtype MinhThiPhan = MinhThiPhan Attrs
  deriving newtype (Show, ToJSON, FromJSON)

minhThiPhan :: MinhThiPhan
minhThiPhan = MinhThiPhan $ baseAttrs
  "03002"
  "Minh Thi Phan"
  Seeker
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 4
    , combat = 2
    , agility = 2
    }
  [Assistant]

instance ActionRunner env => HasActions env MinhThiPhan where
  getActions i window (MinhThiPhan attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env MinhThiPhan where
  runMessage msg i@(MinhThiPhan attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> MinhThiPhan <$> runMessage msg attrs
