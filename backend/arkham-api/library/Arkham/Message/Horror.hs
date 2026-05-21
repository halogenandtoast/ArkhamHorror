{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Horror where

import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

-- | Messages dealing with horror healing and cancellation.
--
-- Note: investigator/asset horror assignment lives elsewhere because it ties
-- into broader damage-assignment flows; this module collects only the simpler
-- heal-and-cancel cluster that pairs naturally with the future Horrorable
-- behavior.
data HorrorMessage
  = HealAllHorror_ Target Source
  | HealHorror_ Target Source Int
  | ExcessHealHorror_ InvestigatorId Source Int
  | CancelHorror_ InvestigatorId Int
  | CancelAssetHorror_ AssetId Source Int
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''HorrorMessage)
