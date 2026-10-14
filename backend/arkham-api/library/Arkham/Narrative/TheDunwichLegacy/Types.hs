{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | What a narrative variant may rewrite in The Dunwich Legacy.

One field per beat whose structure a rewrite can change -- the blocks the
Campaign Guide tells as one piece and a rewrite may split, replace or extend.
'noDunwichNarrative' is the Campaign Guide itself: every beat as printed. A
variant fills in only the beats it changes, so adding a field here costs the
existing variants nothing, and a beat a variant merely rewords needs no field at
all (its prose resolves from the variant's messages at the key the engine already
emits).

The campaign's own code reads these fields off whichever narrative is in play and
never names a variant; see "Arkham.Narrative.TheDunwichLegacy".
-}
module Arkham.Narrative.TheDunwichLegacy.Types where

import Arkham.Helpers.Narrative (Extra, Flavor, FlavorKey)
import Arkham.Prelude

data DunwichNarrative m = DunwichNarrative
  { prologue :: Flavor m
  , armitagesFate1 :: Flavor m
  , interlude1a :: Flavor m
  , interlude2Armitage :: FlavorKey m
  , interlude2FrancisMorgan :: Extra m
  , epilogue :: Flavor m
  , extracurricularActivityIntro :: Flavor m
  , extracurricularActivityResolution2 :: FlavorKey m
  , theHouseAlwaysWinsIntro :: Flavor m
  , theHouseAlwaysWinsResolution3 :: Flavor m
  , theMiskatonicMuseumIntro :: Flavor m
  , theMiskatonicMuseumIntroPart3 :: Extra m
  , theMiskatonicMuseumResolution1 :: FlavorKey m
  , theMiskatonicMuseumResolution2 :: FlavorKey m
  , theEssexCountyExpressIntro :: Flavor m
  , theEssexCountyExpressResolution1 :: Extra m
  , bloodOnTheAltarIntro :: Flavor m
  , bloodOnTheAltarResolution2 :: Extra m
  , undimensionedAndUnseenIntro :: Flavor m
  , undimensionedAndUnseenPart1 :: Flavor m
  , undimensionedAndUnseenPart2 :: Flavor m
  , whereDoomAwaitsIntro :: Flavor m
  , whereDoomAwaitsResolution1 :: Flavor m
  , lostInTimeAndSpaceIntro :: Flavor m
  }

-- | The Campaign Guide: every beat as printed.
noDunwichNarrative :: DunwichNarrative m
noDunwichNarrative =
  DunwichNarrative
    { prologue = Nothing
    , armitagesFate1 = Nothing
    , interlude1a = Nothing
    , interlude2Armitage = Nothing
    , interlude2FrancisMorgan = Nothing
    , epilogue = Nothing
    , extracurricularActivityIntro = Nothing
    , extracurricularActivityResolution2 = Nothing
    , theHouseAlwaysWinsIntro = Nothing
    , theHouseAlwaysWinsResolution3 = Nothing
    , theMiskatonicMuseumIntro = Nothing
    , theMiskatonicMuseumIntroPart3 = Nothing
    , theMiskatonicMuseumResolution1 = Nothing
    , theMiskatonicMuseumResolution2 = Nothing
    , theEssexCountyExpressIntro = Nothing
    , theEssexCountyExpressResolution1 = Nothing
    , bloodOnTheAltarIntro = Nothing
    , bloodOnTheAltarResolution2 = Nothing
    , undimensionedAndUnseenIntro = Nothing
    , undimensionedAndUnseenPart1 = Nothing
    , undimensionedAndUnseenPart2 = Nothing
    , whereDoomAwaitsIntro = Nothing
    , whereDoomAwaitsResolution1 = Nothing
    , lostInTimeAndSpaceIntro = Nothing
    }
