{- | Both rewards are printed on player backs: resolution 2 offers Invisibility
Serum to one investigator, and Griffin's Notes goes to one investigator in
either resolution, if anyone found them.
-}
module Arkham.Homebrew.TheInvisibleMan.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.TheInvisibleMan.Sets qualified as Set

invisibilitySerum :: CardDef
invisibilitySerum =
  ( storyAsset
      ":the-invisible-man:039"
      ("Invisibility Serum" <:> "Perfected, Supposedly")
      2
      Set.TheInvisibleMan
  )
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#combat, #agility, #wild]
    , cdUses = uses Supply 3
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

griffinsNotes :: CardDef
griffinsNotes =
  ( storyAsset
      ":the-invisible-man:040"
      ("Griffin's Notes" <:> "Window to a Maniacal Mind")
      2
      Set.TheInvisibleMan
  )
    { cdCardTraits = setFromList [Item, Tome]
    , cdSkills = [#willpower, #intellect, #wild]
    , cdUses = uses Secret 3
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }
