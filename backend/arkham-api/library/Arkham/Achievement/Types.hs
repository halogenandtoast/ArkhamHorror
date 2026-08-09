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

{- | Return to the Night of the Zealot. Official list: these can only be
completed while playing with the Return to encounter sets (campaign "50").
-}
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

{- | Return to The Path to Carcosa (campaign "52"). Constructor names must stay
globally unique, so shared printed names ("Line in the Sand", "<X>
Expertise") are disambiguated here even though the printed name is not.
-}
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

{- | Return to The Forgotten Age (campaign "53"). The official list gates
these to the Return-to encounter sets only.
-}
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

{- | Return to The Circle Undone (campaign "54"). Constructor names must stay
globally unique, so shared printed names ("<X> Expertise") are disambiguated
here even though the printed name is not.
-}
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

{- | The Drowned City (campaign "11"). The first campaign whose achievement list
is printed for the campaign itself rather than a Return-to variant, so these are
earnable in ordinary Drowned City games.
-}
data TheDrownedCityAchievement
  = OneFirstLastJob
  | SeasonTwo
  | CliffDiver
  | ThisIsACoup
  | ThoroughSearch
  | TidalFlipMinigame
  | NoAcolyteLeftBehind
  | KillTheAdds
  | InTheDeepEnd
  | SorryDidntSeeYouThere
  | SkyRider
  | SkipToTheEnd
  | AlienSchoolDropout
  | AlienSchoolGraduate
  | EmptyHanded
  | WhyWontYouStayDead
  | WithYourPowersCombined
  | Obligations
  | DrownedCityLineInTheSand
  | RlyehExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''TheDrownedCityAchievement)

{- | The Dream-Quest (campaign "06", side A). The Dream-Eaters prints two
achievement lists, one per mini-campaign, and both are earnable in the same
campaign id — the split is a presentation concern ('achievementCampaignPart'),
not a gating one. A few entries here can only be finished by playing the full
interconnected campaign; they are still shown under The Dream-Quest because that
is where they are printed.
-}
data TheDreamQuestAchievement
  = DoYouAlwaysFollowOrders
  | AwwButTheyreSoCute
  | LosingMyReligion
  | FantasyFlightGamesDoesNotCondoneAccomplishingThisAchievement
  | TacticalEspionageAction
  | MoonLizardsIDontBelieveTheyExist
  | BarkhamHorrorEnthusiast
  | OnlyWayToBeSure
  | GiveThemSomethingToTalkAbout
  | ThisIsntEvenMyFinalForm
  | DontTellAnyoneBut
  | DreamQuestLineInTheSand
  | DreamlandsExpertise
  | BewareTheBlackCat
  | ReunitedAndItFeelsSoGood
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''TheDreamQuestAchievement)

-- | The Web of Dreams (campaign "06", side B). See 'TheDreamQuestAchievement'.
data TheWebOfDreamsAchievement
  = EveryonesAFeministUntilThereIsASpiderAround
  | TheCarterMethod
  | TheDoctorIsIn
  | DejaVu
  | TheCasaLomaManeuver
  | IRememberThisPlace
  | BadAdvice
  | MarchOfTheGhouls
  | TheIshimuraFlex
  | YouSpinMeRightRound
  | MasterOfUnlocking
  | WebOfDreamsLineInTheSand
  | UnderworldExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''TheWebOfDreamsAchievement)

{- | The Innsmouth Conspiracy (campaign "07"). Like The Drowned City, this list is
printed for the campaign itself rather than a Return-to variant, so these are
earnable in ordinary Innsmouth games.
-}
data TheInnsmouthConspiracyAchievement
  = WouldYouJustDieAlready
  | ElementaryDearDawson
  | AintNothinGonnaBreakMyStride
  | SpeedingTicket
  | YoureLockedInHereWithMe
  | FishOutOfWater
  | DontWakeDaddy
  | GoneFishing
  | FullBuild
  | YouWakeUpInARoom
  | BiggerFishToFry
  | InnsmouthLineInTheSand
  | InnsmouthExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''TheInnsmouthConspiracyAchievement)

{- | Edge of the Earth (campaign "08"). Printed for the campaign itself rather
than a Return-to variant, so these are earnable in ordinary Edge of the Earth
games.
-}
data EdgeOfTheEarthAchievement
  = SafeBet
  | LookAtAllThisStuff
  | InYourHead
  | ChaosChaos
  | KnockKnock
  | MadWithPower
  | ConstructAdditionalPylons
  | TheSoundOfMadness
  | SorryImAllOutOfDogPuns
  | KindOfAHatOnAHat
  | ThisWasYourIdea
  | NoRespectForTheDead
  | WukWukBoom
  | TheColdNeverBotheredMeAnyway
  | HellFrozeOver
  | AbandonedAndAlone
  | FriendsForever
  | ThereAndBackAgain
  | SnowLineInTheSand
  | AntarcticExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''EdgeOfTheEarthAchievement)

{- | The Scarlet Keys (campaign "09"). Printed for the campaign itself rather
than a Return-to variant, so these are earnable in ordinary Scarlet Keys games.
-}
data TheScarletKeysAchievement
  = CluedIn
  | TakeThatGhulat
  | WhatsInAName
  | PorqueNoLosDos
  | LostAndFound
  | ILikeTowerDefenseGames
  | PlayWithYourFood
  | MoreLikeDestroyedChimera
  | WhoWatchesTheWatcher
  | UnderMyUmbrella
  | AllHollow
  | RedLooksGoodOnMe
  | BloodyRedRevolution
  | ScarletWithYourPowersCombined
  | GiftOfGab
  | ImJustHereForTheLocalCuisine
  | SpeedDemon
  | TrustNobody
  | TrustEverybody
  | HereIsYourBadge
  | KeyToMyHeart
  | ScarletLineInTheSand
  | GlobalExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''TheScarletKeysAchievement)

{- | The Feast of Hemlock Vale (campaign "10"). Printed for the campaign itself
rather than a Return-to variant, so these are earnable in ordinary Hemlock Vale
games.
-}
data TheFeastOfHemlockValeAchievement
  = Aperitif
  | Unshattered
  | AStrongSilentType
  | ColourOutsideTheLines
  | LifeOfTheParty
  | DancingQueen
  | AudreyIII
  | HoldOnToYourPotatoes
  | DreamHomeBreakover
  | SettlingTheScore
  | HereCrabbyCrabby
  | ADifferentKindOfStingOps
  | WaitTheresNoShroudedShrine
  | BearNecessities
  | LetsDoTheTimeWarp
  | OblivionShmoblivion
  | HighDive
  | BestFriendsForever
  | KnowYourPlace
  | HeartOfSteel
  | HoldingOutForAHimbo
  | CaptivatingScream
  | HemlockLineInTheSand
  | HemlockExpertise
  deriving stock (Eq, Show, Ord, Enum, Bounded, Data)

$(deriveJSON defaultOptions ''TheFeastOfHemlockValeAchievement)

data Achievement
  = NightOfTheZealotAchievement NightOfTheZealotAchievement
  | TheDunwichLegacyAchievement TheDunwichLegacyAchievement
  | ThePathToCarcosaAchievement ThePathToCarcosaAchievement
  | TheForgottenAgeAchievement TheForgottenAgeAchievement
  | TheCircleUndoneAchievement TheCircleUndoneAchievement
  | TheDrownedCityAchievement TheDrownedCityAchievement
  | TheDreamQuestAchievement TheDreamQuestAchievement
  | TheWebOfDreamsAchievement TheWebOfDreamsAchievement
  | TheInnsmouthConspiracyAchievement TheInnsmouthConspiracyAchievement
  | EdgeOfTheEarthAchievement EdgeOfTheEarthAchievement
  | TheScarletKeysAchievement TheScarletKeysAchievement
  | TheFeastOfHemlockValeAchievement TheFeastOfHemlockValeAchievement
  deriving stock (Eq, Show, Ord, Data)

allAchievements :: [Achievement]
allAchievements =
  map NightOfTheZealotAchievement [minBound ..]
    <> map TheDunwichLegacyAchievement [minBound ..]
    <> map ThePathToCarcosaAchievement [minBound ..]
    <> map TheForgottenAgeAchievement [minBound ..]
    <> map TheCircleUndoneAchievement [minBound ..]
    <> map TheDrownedCityAchievement [minBound ..]
    <> map TheDreamQuestAchievement [minBound ..]
    <> map TheWebOfDreamsAchievement [minBound ..]
    <> map TheInnsmouthConspiracyAchievement [minBound ..]
    <> map EdgeOfTheEarthAchievement [minBound ..]
    <> map TheScarletKeysAchievement [minBound ..]
    <> map TheFeastOfHemlockValeAchievement [minBound ..]

-- | Flat constructor name; the wire and database representation.
achievementName :: Achievement -> Text
achievementName = \case
  NightOfTheZealotAchievement a -> tshow a
  TheDunwichLegacyAchievement a -> tshow a
  ThePathToCarcosaAchievement a -> tshow a
  TheForgottenAgeAchievement a -> tshow a
  TheCircleUndoneAchievement a -> tshow a
  TheDrownedCityAchievement a -> tshow a
  TheDreamQuestAchievement a -> tshow a
  TheWebOfDreamsAchievement a -> tshow a
  TheInnsmouthConspiracyAchievement a -> tshow a
  EdgeOfTheEarthAchievement a -> tshow a
  TheScarletKeysAchievement a -> tshow a
  TheFeastOfHemlockValeAchievement a -> tshow a

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
  TheDrownedCityAchievement WithYourPowersCombined ->
    Just
      [ "BarrierNode"
      , "GrislyMask"
      , "ObsidianClaw"
      , "TidalTablet"
      , "ShardOfYchlecht"
      , "HorrorInClay"
      ]
  TheDrownedCityAchievement Obligations ->
    Just
      [ "WalkInFaith"
      , "DreamsOfDestruction"
      , "ToeTheLine"
      , "DoNoHarm"
      , "GoodMoney"
      , "NoPlaceLikeHome"
      , "ProveYourWorth"
      , "PlumbTheDepths"
      ]
  TheInnsmouthConspiracyAchievement YouWakeUpInARoom ->
    Just
      [ "AMeetingWithThomasDawson"
      , "ABattleWithAHorrifyingDevil"
      , "ADecisionToStickTogether"
      , "AnEncounterWithASecretCult"
      , "ADealWithJoeSargent"
      , "AFollowedLead"
      , "AnIntervention"
      , "AJailbreak"
      , "DiscoveryOfAStrangeIdol"
      , "DiscoveryOfAnUnholyMantle"
      , "DiscoveryOfAMysticalRelic"
      , "AConversationWithMrMoore"
      , "TheLifecycleOfADeepOne"
      , "AStingingBetrayal"
      , "TheHorribleTruth"
      ]
  EdgeOfTheEarthAchievement ThereAndBackAgain ->
    Just
      [ "DrAmyKensler"
      , "ProfWilliamDyer"
      , "Danforth"
      , "JamesCookieFredericks"
      , "EliyahAshevak"
      , "DrMalaSinha"
      , "TakadaHiroko"
      , "AveryClaypool"
      , "RoaldEllsworth"
      ]
  {- Every ending of the campaign, i.e. every way Fate of the Vale can finish.
  Accumulated across playthroughs by the API layer.
  -}
  TheFeastOfHemlockValeAchievement Unshattered ->
    Just
      [ "MarquezSacrificedHerself"
      , "TheInvestigatorsSacrificedThemselves"
      , "TheValeWasSaved"
      , "TheValeBurned"
      , "BarelySurvivedTheFeast"
      , "BecameTheTrueFeast"
      ]
  -- The five residents "Best Friends Forever!" wants at Relationship Level 6.
  TheFeastOfHemlockValeAchievement BestFriendsForever ->
    Just
      [ "LeahAtwood"
      , "SimeonAtwood"
      , "RiverHawthorne"
      , "GideonMizrah"
      , "WilliamHemlock"
      ]
  -- The eleven Scarlet Keys, in printed checklist order.
  TheScarletKeysAchievement KeyToMyHeart ->
    Just
      [ "TheEyeOfRavens"
      , "TheLastBlossom"
      , "TheLightOfPharos"
      , "TheSableGlass"
      , "TheWeepingLady"
      , "TheTwistedAntiprism"
      , "TheShadeReaper"
      , "TheMirroringBlade"
      , "TheBaleEngine"
      , "TheRuinousChime"
      , "TheWellspringOfFortune"
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
  TheDrownedCityAchievement _ -> ["11"]
  -- Both Dream-Eaters lists live in campaign "06"; the mini-campaign split is
  -- 'achievementCampaignPart', a display grouping only.
  TheDreamQuestAchievement _ -> ["06"]
  TheWebOfDreamsAchievement _ -> ["06"]
  TheInnsmouthConspiracyAchievement _ -> ["07"]
  EdgeOfTheEarthAchievement _ -> ["08"]
  TheScarletKeysAchievement _ -> ["09"]
  TheFeastOfHemlockValeAchievement _ -> ["10"]

{- | Sub-grouping within a campaign, for lists that are printed per mini-campaign.
Only The Dream-Eaters has one: its achievements are split between The Dream-Quest
and The Web of Dreams, and the UI shows them as two sections even when the pair is
played as a single interconnected campaign.
-}
achievementCampaignPart :: Achievement -> Maybe Text
achievementCampaignPart = \case
  TheDreamQuestAchievement _ -> Just "theDreamQuest"
  TheWebOfDreamsAchievement _ -> Just "theWebOfDreams"
  _ -> Nothing

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
