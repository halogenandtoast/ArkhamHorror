{-# LANGUAGE TemplateHaskell #-}

module Arkham.UltimatumsAndBoons.Types where

import Arkham.Homebrew.Ultimatums (homebrewUltimatumNames)
import Arkham.Prelude
import Control.Monad.Fail
import Data.Aeson.TH
import Data.Data (dataTypeConstrs, dataTypeOf, fromConstr, showConstr)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

data Boon
  = BoonOfTheAncients
  | BoonOfAthena
  | BoonOfDestiny
  | BoonOfHades
  | BoonOfHermes
  | BoonOfThoth
  | BoonOfOsiris
  | BoonOfTheMorrigan
  | BoonOfPersephone
  | BoonOfTheExplorer
  | BoonOfTheChild
  | -- Refractions (see 'refractionScope').
    BoonOfAtonement
  | BoonOfBliss
  | BoonOfTheDreamer
  | BoonOfTheMiners
  | BoonOfTheDance
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

data Ultimatum
  = UltimatumOfAgony
  | UltimatumOfBrokenPromises
  | UltimatumOfTheBrokenVeil
  | UltimatumOfChaos
  | UltimatumOfDisaster
  | UltimatumOfDread
  | UltimatumOfFailure
  | UltimatumOfFinality
  | UltimatumOfForbiddenKnowledge
  | UltimatumOfHardship
  | UltimatumOfTheHighlander
  | UltimatumOfInduction
  | UltimatumOfOrthodoxy
  | UltimatumOfTheScream
  | UltimatumOfSurvival
  | UltimatumOfUltimatums
  | UltimatumOfExile
  | UltimatumOfTheSpiral
  | UltimatumOfMalevolence
  | -- Refractions (see 'refractionScope').
    UltimatumOfVenom
  | UltimatumOfAmbuscade
  | UltimatumOfAnnoyance
  | UltimatumOfTheUnspeakableName
  | UltimatumOfTheBrassCrown
  | UltimatumOfTheSleeper
  | UltimatumOfTheFaultyCarburetor
  | UltimatumOfInvisibility
  | UltimatumOfTheMan
  | UltimatumOfSpoilage
  | UltimatumOfDeath
  | UltimatumOfMultiplication
  | UltimatumOfTheDrowned
  | {- | A homebrew campaign's ultimatum. The door for content outside core: the
    'Text' is the full wire name @":\<campaign-id\>:\<Key\>"@, so the campaign
    it belongs to is read off the name. Campaigns declare their lists in their
    own @UltimatumDefs.hs@ (see "Arkham.Homebrew.UltimatumDefs") and implement
    them in their own code.
    -}
    HomebrewUltimatum Text
  deriving stock (Eq, Show, Ord, Data)

data UltimatumOrBoon
  = Ultimatum Ultimatum
  | Boon Boon
  deriving stock (Eq, Show, Ord, Data)

{- | Every non-homebrew ultimatum. Replaces @[minBound .. maxBound]@ now that
'Ultimatum' carries the open 'HomebrewUltimatum' constructor and can no longer
derive 'Enum'. Mirrors 'Arkham.Trait.coreTraits'.
-}
coreUltimatums :: [Ultimatum]
coreUltimatums =
  [ fromConstr con
  | con <- dataTypeConstrs (dataTypeOf (HomebrewUltimatum ""))
  , showConstr con /= "HomebrewUltimatum"
  ]

coreUltimatumsByName :: Map Text Ultimatum
coreUltimatumsByName = Map.fromList [(tshow u, u) | u <- coreUltimatums]

{- | Flat constructor name of the underlying entry (e.g. "BoonOfHades");
doubles as the wire representation and synthetic card code.
-}
variantName :: UltimatumOrBoon -> Text
variantName = \case
  Ultimatum u -> tshow u
  Boon b -> tshow b

allUltimatumsAndBoons :: [UltimatumOrBoon]
allUltimatumsAndBoons =
  map Boon [minBound ..]
    <> map Ultimatum coreUltimatums
    <> map (Ultimatum . HomebrewUltimatum) homebrewUltimatumNames

{- | A homebrew ultimatum by campaign id and key; the campaign-side counterpart
of its @UltimatumDefs.hs@ declaration.
-}
homebrewUltimatum :: Text -> Text -> Ultimatum
homebrewUltimatum campaign key = HomebrewUltimatum (campaign <> ":" <> key)

{- | A Refraction is an Ultimatum or Boon written for one campaign or scenario
rather than for the game at large; the FAQ lists them apart from the general
two. This is the campaign it belongs to, and 'Nothing' for a general entry.

Only a homebrew campaign's own ultimatums are scoped so far; official
Refractions join them here as they are implemented.
-}
refractionScope :: UltimatumOrBoon -> [Text]
refractionScope = \case
  Ultimatum (HomebrewUltimatum t) -> maybeToList (homebrewUltimatumCampaign t)
  Ultimatum UltimatumOfVenom -> theForgottenAge
  Ultimatum UltimatumOfAmbuscade -> theForgottenAge
  Ultimatum UltimatumOfAnnoyance -> ["08"]
  Ultimatum UltimatumOfTheUnspeakableName -> thePathToCarcosa
  Ultimatum UltimatumOfTheBrassCrown -> thePathToCarcosa
  Ultimatum UltimatumOfTheSleeper -> ["11"]
  Ultimatum UltimatumOfTheFaultyCarburetor -> ["07"]
  Ultimatum UltimatumOfInvisibility -> theDunwichLegacy
  Ultimatum UltimatumOfTheMan -> thePathToCarcosa
  Ultimatum UltimatumOfSpoilage -> ["11"]
  Ultimatum UltimatumOfDeath -> thePathToCarcosa
  Ultimatum UltimatumOfMultiplication -> theDunwichLegacy
  Ultimatum UltimatumOfTheDrowned -> ["07"]
  Boon BoonOfTheDreamer -> ["06"]
  Boon BoonOfAtonement -> ["09"]
  Boon BoonOfBliss -> ["10"]
  Boon BoonOfTheMiners -> ["10"]
  Boon BoonOfTheDance -> ["10"]
  _ -> []
 where
  -- A campaign and its Return to are the same campaign for this purpose.
  theForgottenAge = ["04", "53"]
  thePathToCarcosa = ["03", "52"]
  theDunwichLegacy = ["02", "51"]

isRefraction :: UltimatumOrBoon -> Bool
isRefraction = notNull . refractionScope

{- | The campaign id inside a homebrew ultimatum's wire name
(@":dark-matter:UltimatumOfExploration"@ -> @":dark-matter"@).
-}
homebrewUltimatumCampaign :: Text -> Maybe Text
homebrewUltimatumCampaign t = case T.splitOn ":" t of
  ["", campaign, key] | notNull campaign && notNull key -> Just (":" <> campaign)
  _ -> Nothing

{- | Entries excluded from Ultimatum of Ultimatums' per-game roll — its own
text exempts "ultimatums or boons that affect deckbuilding or chaos bag
construction". Boon of the Ancients is included here since campaign-start
experience is meaningless as a single-game roll.
-}
affectsDeckbuildingOrChaosBag :: UltimatumOrBoon -> Bool
affectsDeckbuildingOrChaosBag = \case
  Boon b -> b `elem` [BoonOfTheMorrigan, BoonOfTheAncients]
  Ultimatum u ->
    u
      `elem` [ UltimatumOfBrokenPromises
             , UltimatumOfChaos
             , UltimatumOfDisaster
             , UltimatumOfFailure
             , UltimatumOfTheHighlander
             , UltimatumOfInduction
             , UltimatumOfOrthodoxy
             , UltimatumOfExile
             , UltimatumOfUltimatums -- never rolls itself
             , UltimatumOfAnnoyance
             ]

deriveJSON defaultOptions ''Boon

{- | Core ultimatums serialize as their bare constructor name (as the derived
all-nullary encoding did); a homebrew one serializes as its wire name, which is
always @":campaign:Key"@ and so can never collide with a core name.
-}
instance ToJSON Ultimatum where
  toJSON = \case
    HomebrewUltimatum t -> toJSON t
    u -> toJSON (tshow u)

{- | A homebrew name is recognized by its shape rather than by the registry, so
a saved game still loads after its campaign leaves the build. Anything else is
rejected, which keeps 'FromJSON UltimatumOrBoon''s Boon-then-Ultimatum fallback
honest.
-}
instance FromJSON Ultimatum where
  parseJSON = withText "Ultimatum" \t -> case Map.lookup t coreUltimatumsByName of
    Just u -> pure u
    Nothing
      | ":" `isPrefixOf` t -> pure (HomebrewUltimatum t)
      | otherwise -> fail $ "Unknown ultimatum: " <> unpack t

{- | The union's JSON is deliberately FLAT — @Boon BoonOfHades@ ⇄
@"BoonOfHades"@ — because the wire format predates the union type: the client
sends/reads plain tag strings (create-game POST, settings, Source contents)
and existing saved games store them. A derived tagged encoding would break
all of those. Constructor names are disjoint (BoonOf*/UltimatumOf*), so the
flat form is unambiguous.
-}
instance ToJSON UltimatumOrBoon where
  toJSON = \case
    Ultimatum u -> toJSON u
    Boon b -> toJSON b

instance FromJSON UltimatumOrBoon where
  parseJSON v = (Boon <$> parseJSON v) <|> (Ultimatum <$> parseJSON v)
