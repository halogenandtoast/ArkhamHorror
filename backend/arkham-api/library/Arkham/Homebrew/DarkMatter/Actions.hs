{-# LANGUAGE TemplateHaskell #-}

{- | Actions owned by the Dark Matter campaign.

'declareHomebrewActions' generates a bidirectional pattern synonym for each name
over the core 'Arkham.Action.HomebrewAction' escape hatch, plus an @actions@
aggregate. 'actionAffordability' pairs each action with the 'Criterion' core
evaluates to decide whether it can be taken (the Scan action needs a non-empty
scanning deck). Compilation fails if a name already exists as a core action.
-}
module Arkham.Homebrew.DarkMatter.Actions (module Arkham.Homebrew.DarkMatter.Actions) where

import Arkham.Action (Action)
import Arkham.Criteria (Criterion (ScenarioDeckWithCard))
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Homebrew.TH (declareHomebrewActions)

declareHomebrewActions
  [ "Scan"
  ]

-- | The Scan action is affordable only while the scanning deck has cards.
actionAffordability :: [(Action, Criterion)]
actionAffordability =
  [ (Scan, ScenarioDeckWithCard ScanningDeck)
  ]
