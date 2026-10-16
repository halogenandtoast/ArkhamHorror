{- | Your Other Half is the id Dr. Jekyll's formula lets loose: a bonded
investigator card that replaces your own while Duplicitous Drug holds it in
play.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Investigators where

import Arkham.Card.CardDef
import Arkham.ClassSymbol
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Sets qualified as Set
import Arkham.Investigator.Cards.Base
import Arkham.Name
import Arkham.Prelude
import Arkham.Trait

yourOtherHalf :: CardDef
yourOtherHalf =
  ( investigator
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:035"
      ("Your Other Half" <:> "The Id Unleashed")
      Neutral
      [Lunatic]
  )
    { cdEncounterSet = Just Set.TheStrangeCaseOfDrJekyllAndMrHyde
    , cdOtherSide = Just ":the-strange-case-of-dr-jekyll-and-mr-hyde:035b"
    }
