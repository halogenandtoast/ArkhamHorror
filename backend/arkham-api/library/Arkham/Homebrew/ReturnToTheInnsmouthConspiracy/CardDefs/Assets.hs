module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Set

{- | return_to_the_vanishing_of_elina_harper. The three Hybrid story allies shuffled into
the Leads deck, so they are drawn as encounter cards and have an encounter back.
-}
littleGemma :: CardDef
littleGemma =
  ( encounterAsset
      ":return-to-the-innsmouth-conspiracy:025"
      ("\"Little\" Gemma" <:> "Sees More Than She Lets On")
      0
      Set.ReturnToTheVanishingOfElinaHarper
  )
    { cdCardTraits = setFromList [Ally, Hybrid]
    , cdUnique = True
    }

ronStalwick :: CardDef
ronStalwick =
  ( encounterAsset
      ":return-to-the-innsmouth-conspiracy:026"
      ("Ron Stalwick" <:> "Spinning His Yarn")
      0
      Set.ReturnToTheVanishingOfElinaHarper
  )
    { cdCardTraits = setFromList [Ally, Hybrid]
    , cdUnique = True
    }

roderick :: CardDef
roderick =
  ( encounterAsset
      ":return-to-the-innsmouth-conspiracy:027"
      ("Roderick" <:> "\"Just Roderick\"")
      0
      Set.ReturnToTheVanishingOfElinaHarper
  )
    { cdCardTraits = setFromList [Ally, Hybrid]
    , cdUnique = True
    }

-- | return_to_devil_reef. Replaces the Fishing Vessel story asset (07178).
fishingVesselV2 :: CardDef
fishingVesselV2 =
  ( encounterAsset_
      ":return-to-the-innsmouth-conspiracy:034"
      ("Fishing Vessel" <:> "Riding the Waves")
      Set.ReturnToDevilReef
  )
    { cdCardTraits = setFromList [Vehicle]
    , cdUnique = True
    }
