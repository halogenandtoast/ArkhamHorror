{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Doom where

import Arkham.Matcher (RemoveDoomMatchers)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

-- | Messages dealing with doom placement, removal, and flipping.
--
-- Note: 'PlaceDoomOnAgenda' is NOT extracted here. It references @CanAdvance@,
-- which is defined inline in "Arkham.Message" and would require a non-trivial
-- type relocation to extract cleanly.
data DoomMessage
  = RemoveAllDoom_ Source Target
  | RemoveAllDoomFromPlay_ RemoveDoomMatchers
  | FlipDoom_ Target Int
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''DoomMessage)
