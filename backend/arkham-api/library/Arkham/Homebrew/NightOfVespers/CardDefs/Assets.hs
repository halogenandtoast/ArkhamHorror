module Arkham.Homebrew.NightOfVespers.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.NightOfVespers.Sets qualified as Set

finlayODoyle :: CardDef
finlayODoyle =
  (storyAsset ":night-of-vespers:032" ("Finlay O'Doyle" <:> "Faithful Watchdog") 3 Set.TheEndOfSeasons)
    { cdCardTraits = setFromList [Ally, Criminal, HomebrewTrait "O'Bannion"]
    , cdSkills = [#combat, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    }

gigiGiuliano :: CardDef
gigiGiuliano =
  ( storyAsset
      ":night-of-vespers:033"
      ("Gigi Giuliano" <:> "Been Things, Seen Places")
      3
      Set.TheEndOfSeasons
  )
    { cdCardTraits = setFromList [Ally, Criminal, Syndicate]
    , cdSkills = [#agility, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    }

lachrymarumsFavor :: CardDef
lachrymarumsFavor =
  ( storyWeakness
      ":night-of-vespers:124"
      ("Lachrymarum's Favor" <:> "A Veil of Tears")
      Set.TheTeachingsOfAradia
  )
    { cdCardTraits = singleton Omen
    , cdSlots = [#arcane]
    , cdEncounterSetQuantity = Just 4
    }

suspiriorumsFavor :: CardDef
suspiriorumsFavor =
  ( storyWeakness
      ":night-of-vespers:125"
      ("Suspiriorum's Favor" <:> "Until Your Last Gasp")
      Set.TheTeachingsOfAradia
  )
    { cdCardTraits = singleton Omen
    , cdSlots = [#arcane]
    , cdEncounterSetQuantity = Just 4
    }

tenebrarumsFavor :: CardDef
tenebrarumsFavor =
  ( storyWeakness
      ":night-of-vespers:126"
      ("Tenebrarum's Favor" <:> "Under a Shroud of Darkness")
      Set.TheTeachingsOfAradia
  )
    { cdCardTraits = singleton Omen
    , cdSlots = [#arcane]
    , cdEncounterSetQuantity = Just 4
    }
