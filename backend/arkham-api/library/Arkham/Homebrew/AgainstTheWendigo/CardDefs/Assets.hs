{- | Against the Wendigo's assets.

Expedition Notebook, Sarcee Guide, Ithaqua's Knowledge and the Tomahawk are set
aside on *player* backs, because the epilogue can hand some of them to an
investigator's deck. Lost Child is drawn from the encounter deck, so it keeps
the encounter back. The three story-card backs follow whichever back their front
is printed on.
-}
module Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.AgainstTheWendigo.Sets qualified as Set
import Arkham.Homebrew.AgainstTheWendigo.Traits

expeditionNotebook :: CardDef
expeditionNotebook =
  ( storyAsset_
      ":against-the-wendigo:030"
      ("Expedition Notebook" <:> "Dr. Nadelmann's Lost Log")
      Set.HanninahValley
  )
    { cdCardTraits = setFromList [Item, Guide]
    , cdUnique = True
    }

sarceeGuide :: CardDef
sarceeGuide =
  (storyAsset_ ":against-the-wendigo:031" "Sarcee Guide" Set.HanninahValley)
    { cdCardTraits = setFromList [Ally, Guide, Sarcee]
    , cdUnique = True
    }

ithaquasKnowledge :: CardDef
ithaquasKnowledge =
  permanent
    $ (storyAsset_ ":against-the-wendigo:032" "Ithaqua's Knowledge" Set.HanninahValley)
      { cdCardTraits = setFromList [Occult, Spirit]
      }

tomahawk :: CardDef
tomahawk =
  fast
    $ ( storyAsset
          ":against-the-wendigo:033"
          ("Tomahawk" <:> "Foxtail's Ancestral Weapon")
          3
          Set.HanninahValley
      )
      { cdCardTraits = setFromList [Item, Weapon, Melee, Relic]
      , cdSkills = [#combat, #agility, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

lostChild :: CardDef
lostChild =
  (encounterAsset_ ":against-the-wendigo:040" "Lost Child" Set.HanninahValley)
    { cdCardTraits = setFromList [Ally, Sarcee]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

-- Story-card backs.

-- | The back of Hanninah's Gold; the epilogue can add it to a deck.
goldMiningRevenues :: CardDef
goldMiningRevenues =
  permanent
    $ ( storyAsset_
          ":against-the-wendigo:021b"
          ("Gold Mining Revenues" <:> "Money Has No Smell")
          Set.HanninahValley
      )
      { cdCardTraits = setFromList [Condition]
      , cdOtherSide = Just ":against-the-wendigo:021"
      }

-- | The back of Charlie Foxtail's Destiny.
charlieFoxtail :: CardDef
charlieFoxtail =
  ( encounterAsset_
      ":against-the-wendigo:022b"
      ("Charlie Foxtail" <:> "Wants to Regain His Honor")
      Set.HanninahValley
  )
    { cdCardTraits = setFromList [Ally, Guide, Sarcee]
    , cdUnique = True
    , cdOtherSide = Just ":against-the-wendigo:022"
    }

-- | The back of Norman's Fate (v. II).
normanFalkner :: CardDef
normanFalkner =
  ( encounterAsset_
      ":against-the-wendigo:027b"
      ("Norman Falkner" <:> "Not Himself Anymore")
      Set.HanninahValley
  )
    { cdCardTraits = setFromList [Ally, Guide]
    , cdUnique = True
    , cdOtherSide = Just ":against-the-wendigo:027"
    }
