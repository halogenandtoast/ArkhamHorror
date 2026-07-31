{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator (module Arkham.Investigator.Types, withInvestigatorCardCode, lookupInvestigator) where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Investigator.Types
import Arkham.Card.CardCode
import Arkham.Id

instance FromJSON Investigator

withInvestigatorCardCode :: CardCode -> (SomeInvestigator -> r) -> r

lookupInvestigator :: InvestigatorId -> PlayerId -> Investigator
