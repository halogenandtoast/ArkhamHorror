{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.HarveyWalters where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype HarveyWaltersI = HarveyWaltersI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

harveyWalters :: HarveyWaltersI
harveyWalters = HarveyWaltersI $ baseAttrs
  "60201"
  "Harvey Walters"
  Stats
    { health = 7
    , sanity = 8
    , willpower = 4
    , intellect = 5
    , combat = 1
    , agility = 2
    }
  [Miskatonic]

instance (InvestigatorRunner env) => RunMessage env HarveyWaltersI where
  runMessage msg i@(HarveyWaltersI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> HarveyWaltersI <$> runMessage msg attrs
