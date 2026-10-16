{- | Henry Jekyll keeps the encounter back -- setup puts him into play at
Northside Plaza and he never leaves, flipping back and forth with the Edward
Hyde enemy printed on his far side. The other two are printed on player backs
and are offered by the resolutions.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Sets qualified as Set

henryJekyll :: CardDef
henryJekyll =
  ( encounterAsset_
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:020"
      ("Henry Jekyll" <:> "Last Bastion of Morality")
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
  )
    { cdCardTraits = singleton Medic
    , cdUnique = True
    , cdOtherSide = Just ":the-strange-case-of-dr-jekyll-and-mr-hyde:020b"
    , cdDoubleSided = True
    }

duplicitousDrug :: CardDef
duplicitousDrug =
  ( storyAsset
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:034"
      ("Duplicitous Drug" <:> "Search Your Soul")
      3
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
  )
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#wild, #wild]
    , cdUnique = True
    , cdUses = uses Dose 3
    , cdBondedWith = [(1, ":the-strange-case-of-dr-jekyll-and-mr-hyde:035")]
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

fundamentalFormula :: CardDef
fundamentalFormula =
  ( storyAsset
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:036"
      ("Fundamental Formula" <:> "Just What Are You?")
      1
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
  )
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#wild, #wild]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }
