{-# LANGUAGE TemplateHaskell #-}

{- | Achievements ("above the table"): per-user, cross-game accomplishments
from the official campaign achievement lists.

Discrete per-campaign enums merged into 'Achievement' (the Ultimatums & Boons
pattern). The wire/database representation is the flat constructor name —
'PersistField' stores it directly as text, so rows read back as typed values.
Constructor names must therefore stay unique across ALL campaign enums; when
a later campaign reuses a printed name (every campaign has a "Line in the
Sand"), disambiguate the constructor, not the wire format.

Detection lives with each campaign (e.g.
"Arkham.Campaign.Campaigns.NightOfTheZealot" pushes 'EarnAchievement'); the
API layer persists earns per human player and pushes the unlock toast.
-}
module Arkham.Achievement.Types where

import Arkham.Prelude
import Control.Monad.Fail
import Data.Aeson.TH
import Database.Persist.Sql

-- | Return to the Night of the Zealot. Official list: these can only be
-- completed while playing with the Return to encounter sets (campaign "50").
data NightOfTheZealotAchievement
  = TheZealotsRevenge
  | IDontTrustHer
  | InsuranceDoesntCoverGhouls
  | ButDoIHaveTo
  | ConspiracyOfSilence
  | TourOfArkham
  | DoYouGetItNow
  | PinchHitter
  | EvenDeathMayDie
  | BreakTheCircle
  | TheyreJustMisunderstood
  | UmordhothsFavor
  | ZealotLineInTheSand
  | ArkhamExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''NightOfTheZealotAchievement)

-- | Return to the Dunwich Legacy (campaign "51").
data TheDunwichLegacyAchievement
  = WhatIsThisStuffAnyway
  | NoVoidForYou
  | FirstRuleOfArkham
  | AllAboard
  | TheGangsAllHere
  | NoBroodLeftBehind
  | RemindMeNotToPissHerOff
  | Eureka
  | BeyondWhatVeil
  | HereWeGoAgain
  | BirdHunting
  | TheyArentGettingAwayWithThis
  | TabulaRasa
  | DunwichLineInTheSand
  | DunwichExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''TheDunwichLegacyAchievement)

-- | Return to The Path to Carcosa (campaign "52"). Constructor names must stay
-- globally unique, so shared printed names ("Line in the Sand", "<X>
-- Expertise") are disambiguated here even though the printed name is not.
data ThePathToCarcosaAchievement
  = FairWarning
  | FirstSteps
  | CrashingTheParty
  | ForPryingEyes
  | TheCuckoosNest
  | TakeALookAtThis
  | ThePathOfDeath
  | GuessingGame
  | HasturMadeMeDoIt
  | SayMyName
  | GetBackHere
  | ThePathIsFalse
  | ThePathIsReal
  | ThePathIsMine
  | CarcosaLineInTheSand
  | CarcosaExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''ThePathToCarcosaAchievement)

-- | Return to The Forgotten Age (campaign "53"). The official list gates
-- these to the Return-to encounter sets only.
data TheForgottenAgeAchievement
  = WhyDidItHaveToBeSnakes
  | WatchThemUnravel
  | HopeForHumanity
  | Scenario5What
  | BeyondPerfection
  | IRememberEverything
  | Patricide
  | HesGotAPoint
  | ValusiaSoundsGreat
  | IveBuiltUpAnImmunity
  | WeHaveAnUnderstanding
  | WhoNeedsAnyOfThisJunk
  | DontTreadOnMe
  | BaneOfYig
  | IfICouldTurnBackTime
  | YothExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''TheForgottenAgeAchievement)

-- | Return to The Circle Undone (campaign "54"). Constructor names must stay
-- globally unique, so shared printed names ("<X> Expertise") are disambiguated
-- here even though the printed name is not.
data TheCircleUndoneAchievement
  = WhoYouGonnaCall
  | SaviorOfHumanity
  | TenOutOfTenWouldReadAgain
  | CarlShmarl
  | TheThreefoldRule
  | NewWorldOrder
  | ImmortalitySoundsNice
  | MoreLikeExcursion
  | MemberThese
  | CaseClosed
  | MusicOfTheOuterGods
  | WeaverOfShadowAndMist
  | FinePrint
  | SpeakTheWordsAloud
  | CircleExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''TheCircleUndoneAchievement)

data Achievement
  = NightOfTheZealotAchievement NightOfTheZealotAchievement
  | TheDunwichLegacyAchievement TheDunwichLegacyAchievement
  | ThePathToCarcosaAchievement ThePathToCarcosaAchievement
  | TheForgottenAgeAchievement TheForgottenAgeAchievement
  | TheCircleUndoneAchievement TheCircleUndoneAchievement
  deriving stock (Eq, Show, Ord, Data)

allAchievements :: [Achievement]
allAchievements =
  map NightOfTheZealotAchievement [minBound ..]
    <> map TheDunwichLegacyAchievement [minBound ..]
    <> map ThePathToCarcosaAchievement [minBound ..]
    <> map TheForgottenAgeAchievement [minBound ..]
    <> map TheCircleUndoneAchievement [minBound ..]

-- | Flat constructor name; the wire and database representation.
achievementName :: Achievement -> Text
achievementName = \case
  NightOfTheZealotAchievement a -> tshow a
  TheDunwichLegacyAchievement a -> tshow a
  ThePathToCarcosaAchievement a -> tshow a
  TheForgottenAgeAchievement a -> tshow a
  TheCircleUndoneAchievement a -> tshow a

parseAchievement :: Text -> Maybe Achievement
parseAchievement t = lookup t achievementsByName
 where
  achievementsByName = map (achievementName &&& id) allAchievements

{- | Checklist achievements tracked item-by-item across playthroughs. The
items are stable wire keys: detection code reports them via
'AchievementProgress', the API layer accumulates them in the row's
@progress@ column, and the earn happens when every item is checked. The
frontend mirrors these keys for the checklist breakdown and i18n names.
-}
achievementChecklist :: Achievement -> Maybe [Text]
achievementChecklist = \case
  TheDunwichLegacyAchievement TheGangsAllHere ->
    Just
      [ "DrHenryArmitage"
      , "ZebulonWhateley"
      , "DrFrancisMorgan"
      , "EarlSawyer"
      , "ProfessorWarrenRice"
      ]
  ThePathToCarcosaAchievement FirstSteps ->
    Just
      [ "ConstanceDumaine"
      , "SebastienMoreau"
      , "JordanPerry"
      , "AshleighClarke"
      , "IshimaruHaruko"
      ]
  TheCircleUndoneAchievement MemberThese ->
    Just
      [ "MesmerizingFlute"
      , "RitualComponents"
      , "ScrapOfTornShadow"
      , "StrangeIncantation"
      , "GilmansJournal"
      , "KeziahsFormulae"
      , "WornCrucifix"
      , "WispOfSpectralMist"
      , "CornHuskDoll"
      , "BloodyTreeCarvings"
      ]
  TheCircleUndoneAchievement CaseClosed ->
    Just
      [ "ValentinoRivas"
      , "GavriellaMizrah"
      , "PennyWhite"
      , "JeromeDavids"
      ]
  _ -> Nothing

-- | Campaign ids this achievement can be earned in.
achievementCampaigns :: Achievement -> [Text]
achievementCampaigns = \case
  NightOfTheZealotAchievement _ -> ["50"]
  TheDunwichLegacyAchievement _ -> ["51"]
  ThePathToCarcosaAchievement _ -> ["52"]
  TheForgottenAgeAchievement _ -> ["53"]
  TheCircleUndoneAchievement _ -> ["54"]

-- Flat JSON, mirroring UltimatumOrBoon: the union never leaks its shape.
instance ToJSON Achievement where
  toJSON = toJSON . achievementName

instance FromJSON Achievement where
  parseJSON = withText "Achievement" \t ->
    maybe (fail $ "Unknown achievement: " <> unpack t) pure (parseAchievement t)

instance PersistField Achievement where
  toPersistValue = PersistText . achievementName
  fromPersistValue = \case
    PersistText t ->
      maybe (Left $ "Unknown achievement: " <> t) Right (parseAchievement t)
    other -> Left $ "Achievement must be text, got: " <> tshow other

instance PersistFieldSql Achievement where
  sqlType _ = SqlString
