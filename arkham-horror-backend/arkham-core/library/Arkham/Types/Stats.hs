module Arkham.Types.Stats
  ( Stats(..)
  )
where

import ClassyPrelude

data Stats = Stats { health :: Int, sanity :: Int, willpower :: Int, intellect :: Int, combat :: Int, agility :: Int }
