module Arkham.Campaigns.TheFeastOfHemlockVale.Key where

import Arkham.Prelude

data AreasSurveyed
  = NorthPointMine
  | HemlockHarbor
  | PearlRidge
  | AkwanShoreline
  | EastwickBog
  | WesternWoods
  | SouthernFields
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data SimeonAtwoodNotes
  = SimeonSurvived
  | SimeonDisappeared
  | SimeonHatchedAPlan
  | ThePlanIsUnderway
  | SimeonSharedADance
  | SimeonStoodByYou
  | SimeonAtwoodRelationshipLevel
  | SimeonSacrificedThemselvesForTheInvestigators
  | SimeonCrossedOut
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data LeahAtwoodNotes
  = LeahSawSomethingInTheMine
  | LeahAndSimeonWereReunited
  | LeahSearchedThePearlRuins
  | LeahSharedHerFrustrations
  | LeahIsSearchingForSimeon
  | LeahSharedADance
  | LeahStoodByYou
  | LeahAtwoodRelationshipLevel
  | LeahSacrificedThemselvesForTheInvestigators
  | LeahCrossedOut
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data TheoPetersNotes
  = TheoReconciledWithHelen
  | TheoDistractedTheBear
  | TheoSharedADance
  | TheoRejectedHisFamily
  | TheoStoodByYou
  | TheoIsHavingSecondThoughts
  | TheoPetersRelationshipLevel
  | TheoSacrificedThemselvesForTheInvestigators
  | TheoCrossedOut
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data GideonMizrahNotes
  = GideonFoundHisTreasure
  | GideonToldTheStoryOfCaptainHemlock
  | GideonToldTheTaleOfTheAnnabelleLee
  | GideonSharedADance
  | GideonStoodByYou
  | GideonMizrahRelationshipLevel
  | GideonSacrificedThemselvesForTheInvestigators
  | GideonCrossedOut
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data JudithParkNotes
  = JudithSharedAGrudge
  | JudithSavedYourAss
  | JudithSharedADance
  | JudithStoodByYou
  | YouBackedJudithUp
  | JudithParkRelationshipLevel
  | JudithSacrificedThemselvesForTheInvestigators
  | JudithCrossedOut
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data WilliamHemlockNotes
  = WilliamSharedHisLegacy
  | WilliamTookHeart
  | WilliamSharedADance
  | WilliamStoodByYou
  | WilliamIsResolved
  | WilliamHemlockRelationshipLevel
  | WilliamSacrificedThemselvesForTheInvestigators
  | WilliamCrossedOut
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data RiverHawthorneNotes
  = RiverSharedTheirAmbitions
  | TheSchemeIsInMotion
  | RiverSharedADance
  | RiverStoodByYou
  | RiverIsReclaimingTheirLegacy
  | RiverHawthorneRelationshipLevel
  | RiverSacrificedThemselvesForTheInvestigators
  | RiverCrossedOut
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data MotherRachelNotes
  = MotherRachelIntervened
  | MotherRachelSharedHerDoubts
  | MotherRachelRelationshipLevel
  | MotherRachelCrossedOut
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data TheFeastOfHemlockValeKey
  = TheInvestigatorsSurvivedTheHorrorsInTheRock
  | TheInvestigatorsLaidThePearlFamilyToRest
  | TheRemainsWerePartiallyRecovered
  | MadamePearlsDiaryWasRecovered
  | ThePetersFamilyWereReunited
  | ElizabethPetersWasSaved
  | TheChelydranHybridPerished
  | TheChelydranHybridLived
  | TheChelydranHybridWasDevoured
  | TheThingInTheDepthsWasDefeated
  | FinishedTheirMeal
  | HelenPetersJoinedTheSurvey
  | MotherRachelShowedTheWay
  | TheInvestigatorsLostThePath
  | BertieWasLostInTheWoods
  | BertieWasRescued
  | TheBearWasWounded
  | HelenSharedADance
  | BertieHadAnEpiphany
  | TheInvestigatorsFacedTheLongestNightAlone
  | TheCaptivesWereSaved
  | ManyCaptivesWereLost
  | AllTheCaptivesWereLost
  | DrMarquezHasAPlan
  | TheInvestigatorsLearnedTheirPlace
  | DrMarquesHasAHunch
  | TheValeIsFullOfFireworks
  | TheInvestigatorsBelieved
  | TheInvestigatorsLiedToMotherRachel
  | TheInvestigatorsInterruptedTheFeast
  | GideonFinishedTheTaleOfAnnabelleLee
  | TheHemlocksMadeATruce
  | BertiePerished
  | TheInvestigatorsBecameTheTrueFeastOfHemlockVale
  | DrMarquezSacrificedHerselfForTheVale
  | TheInvestigatorsSacrificedThemselvesForTheVale
  | TheValeWasSaved
  | TheValeBurned
  | TheInvestigatorsBarelySurvivedTheFeastOfHemlockVale
  | HelenSacrificedThemselvesForTheInvestigators
  | SimeonAtwoodNotes SimeonAtwoodNotes
  | LeahAtwoodNotes LeahAtwoodNotes
  | TheoPetersNotes TheoPetersNotes
  | GideonMizrahNotes GideonMizrahNotes
  | JudithParkNotes JudithParkNotes
  | WilliamHemlockNotes WilliamHemlockNotes
  | RiverHawthorneNotes RiverHawthorneNotes
  | MotherRachelNotes MotherRachelNotes
  | AreasSurveyed AreasSurveyed
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
