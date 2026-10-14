{-# LANGUAGE NoFieldSelectors #-}

{- | What a narrative variant may rewrite in The Innsmouth Conspiracy.

One field per beat whose structure a rewrite can change -- the blocks the
Campaign Guide tells as one piece and a rewrite may split, replace, extend or
leave untold. 'noInnsmouthNarrative' is the Campaign Guide itself: every beat as
printed. A variant fills in only the beats it changes, so adding a field here
costs the existing variants nothing, and a beat a variant merely rewords needs no
field at all (its prose resolves from the variant's messages at the key the
engine already emits).

The campaign's own code reads these fields off whichever narrative is in play and
never names a variant; see "Arkham.Narrative.TheInnsmouthConspiracy".
-}
module Arkham.Narrative.TheInnsmouthConspiracy.Types where

import Arkham.Helpers.Narrative (Extra, Flavor, FlavorKey, Omit)
import Arkham.Prelude

data InnsmouthNarrative m = InnsmouthNarrative
  { thePitOfDespairFlashback1 :: Flavor m
  , interlude1Part2 :: Flavor m
  , theVanishingOfElinaHarperIntro1 :: Flavor m
  , omitsTheVanishingOfElinaHarperIntro2 :: Omit
  , inTooDeepFlashback6 :: Flavor m
  , inTooDeepFlashback8 :: Flavor m
  , interlude3NoKeys :: Extra m
  , interlude4HiddenTruths :: Flavor m
  , interlude4RightfulKeeper :: FlavorKey m
  , intoTheMaelstromIntro :: Flavor m
  , omitsEpilogue2 :: Omit
  }

-- | The Campaign Guide: every beat as printed.
noInnsmouthNarrative :: InnsmouthNarrative m
noInnsmouthNarrative =
  InnsmouthNarrative
    { thePitOfDespairFlashback1 = Nothing
    , interlude1Part2 = Nothing
    , theVanishingOfElinaHarperIntro1 = Nothing
    , omitsTheVanishingOfElinaHarperIntro2 = False
    , inTooDeepFlashback6 = Nothing
    , inTooDeepFlashback8 = Nothing
    , interlude3NoKeys = Nothing
    , interlude4HiddenTruths = Nothing
    , interlude4RightfulKeeper = Nothing
    , intoTheMaelstromIntro = Nothing
    , omitsEpilogue2 = False
    }
