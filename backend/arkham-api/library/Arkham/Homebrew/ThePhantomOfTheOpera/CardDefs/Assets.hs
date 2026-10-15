{- | Christine keeps the encounter back -- act 4 puts her into play at the
Phantom's location and she only ever leaves for the victory display. The other
three are printed on player backs: act 1 hands the Persian to an investigator,
and the resolutions offer all three as rewards.
-}
module Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.ThePhantomOfTheOpera.Sets qualified as Set
import Arkham.Homebrew.ThePhantomOfTheOpera.Traits ()

thePersian :: CardDef
thePersian =
  ( storyAsset
      ":the-phantom-of-the-opera:024"
      ("The Persian" <:> "Mysterious Confidant")
      4
      Set.ThePhantomOfTheOpera
  )
    { cdCardTraits = setFromList [Ally, Conspirator]
    , cdSkills = [#combat, #intellect, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

christineDaae :: CardDef
christineDaae =
  ( encounterAsset_
      ":the-phantom-of-the-opera:025"
      ("Christine Daaé" <:> "Diva of the Opéra Garnier")
      Set.ThePhantomOfTheOpera
  )
    { cdCardTraits = setFromList [Bystander, Performer]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

theScorpion :: CardDef
theScorpion =
  ( storyAsset
      ":the-phantom-of-the-opera:026"
      ("The Scorpion" <:> "Damned If You Do")
      1
      Set.ThePhantomOfTheOpera
  )
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#combat, #intellect, #wild]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

theGrasshopper :: CardDef
theGrasshopper =
  ( storyAsset
      ":the-phantom-of-the-opera:027"
      ("The Grasshopper" <:> "Damned If You Don't")
      1
      Set.ThePhantomOfTheOpera
  )
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#agility, #willpower, #wild]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }
