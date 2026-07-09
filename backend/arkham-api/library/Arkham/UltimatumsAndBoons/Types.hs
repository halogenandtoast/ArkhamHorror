{-# LANGUAGE TemplateHaskell #-}

module Arkham.UltimatumsAndBoons.Types where

import Arkham.Prelude
import Data.Aeson.TH

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
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

data UltimatumOrBoon
  = Ultimatum Ultimatum
  | Boon Boon
  deriving stock (Eq, Show, Ord, Data)

-- | Flat constructor name of the underlying entry (e.g. "BoonOfHades");
-- doubles as the wire representation and synthetic card code.
variantName :: UltimatumOrBoon -> Text
variantName = \case
  Ultimatum u -> tshow u
  Boon b -> tshow b

allUltimatumsAndBoons :: [UltimatumOrBoon]
allUltimatumsAndBoons = map Boon [minBound ..] <> map Ultimatum [minBound ..]

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
             ]

mconcat
  [ deriveJSON defaultOptions ''Boon
  , deriveJSON defaultOptions ''Ultimatum
  ]

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
