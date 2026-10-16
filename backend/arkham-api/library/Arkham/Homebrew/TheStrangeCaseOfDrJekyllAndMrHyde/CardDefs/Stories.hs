{- | Adapted Formula is the story half of a two-sided card: act 1 puts it into
play beside the act deck, and its own {fast} ability flips it to the Sinister
Chemist enemy printed on the back.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Stories where

import Arkham.Card.CardCode (flippedCardCode)
import Arkham.Card.CardDef
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Sets qualified as Set
import Arkham.Prelude
import Arkham.Story.CardDefs.Base

adaptedFormula :: CardDef
adaptedFormula =
  ( story
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:006"
      "Adapted Formula"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
  )
    { cdOtherSide = Just (flippedCardCode ":the-strange-case-of-dr-jekyll-and-mr-hyde:006")
    }
