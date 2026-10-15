{- | Roderick and Madeline are encounter-backed: they are put into play by the
scenario and never enter a deck. Faded Silhouette and the two Mad Trists are
printed on player backs -- Resolution 2 offers them as rewards.
-}
module Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

roderickUsher :: CardDef
roderickUsher =
  ( encounterAsset_
      ":the-fall-of-the-house-of-usher:051"
      ("Roderick Usher" <:> "Last of the Line of Usher")
      Set.TheFallOfTheHouseOfUsher
  )
    { cdCardTraits = setFromList [Patron, Cursed]
    , cdUnique = True
    }

madelineUsher :: CardDef
madelineUsher =
  ( encounterAsset_
      ":the-fall-of-the-house-of-usher:052"
      ("Madeline Usher" <:> "Neither Living Nor Dead")
      Set.TheFallOfTheHouseOfUsher
  )
    { cdCardTraits = setFromList [Geist, Cursed]
    , cdUnique = True
    }

fadedSilhouette :: CardDef
fadedSilhouette =
  ( storyAsset_
      ":the-fall-of-the-house-of-usher:053"
      ("Faded Silhouette" <:> "Memento Mori")
      Set.TheFallOfTheHouseOfUsher
  )
    { cdCardTraits = setFromList [Item, Relic]
    , cdKeywords = singleton Keyword.Permanent
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

theMadTristShelterFromTheStorm :: CardDef
theMadTristShelterFromTheStorm =
  ( storyAsset
      ":the-fall-of-the-house-of-usher:054"
      ("The Mad Trist" <:> "Shelter From the Storm")
      2
      Set.TheFallOfTheHouseOfUsher
  )
    { cdCardTraits = setFromList [Item, Tome, Blessed]
    , cdSkills = [#willpower, #combat, #wild]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

theMadTristDragonsCunning :: CardDef
theMadTristDragonsCunning =
  ( storyAsset
      ":the-fall-of-the-house-of-usher:055"
      ("The Mad Trist" <:> "Dragon's Cunning")
      2
      Set.TheFallOfTheHouseOfUsher
  )
    { cdCardTraits = setFromList [Item, Tome, Cursed]
    , cdSkills = [#intellect, #agility, #wild]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }
