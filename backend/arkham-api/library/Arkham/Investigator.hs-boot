{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator (module Arkham.Investigator.Types) where

import Arkham.Classes.RunMessage.Internal
import Arkham.Prelude

import {-# SOURCE #-} Arkham.Investigator.Types

instance FromJSON Investigator
instance RunMessage Investigator
