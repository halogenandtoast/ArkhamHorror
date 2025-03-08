{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator (module Arkham.Investigator.Types, withInvestigatorCardCode) where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Investigator.Types
import Arkham.Card.CardCode

instance FromJSON Investigator

withInvestigatorCardCode :: CardCode -> (SomeInvestigator -> r) -> r
