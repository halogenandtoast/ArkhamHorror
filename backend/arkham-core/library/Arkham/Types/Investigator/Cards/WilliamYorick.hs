{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.WilliamYorick where

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

newtype WilliamYorick = WilliamYorick Attrs
  deriving newtype (Show, ToJSON, FromJSON)

williamYorick :: WilliamYorick
williamYorick = WilliamYorick $ baseAttrs
  "03005"
  "William Yorick"
  Survivor
  Stats
    { health = 8
    , sanity = 6
    , willpower = 3
    , intellect = 2
    , combat = 4
    , agility = 3
    }
  [Warden]

instance ActionRunner env => HasActions env WilliamYorick where
  getActions i window (WilliamYorick attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env WilliamYorick where
  runMessage msg i@(WilliamYorick attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> WilliamYorick <$> runMessage msg attrs
