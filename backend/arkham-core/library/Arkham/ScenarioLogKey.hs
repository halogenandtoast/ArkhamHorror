{-# LANGUAGE TemplateHaskell #-}

module Arkham.ScenarioLogKey where

import Arkham.Prelude hiding (toLower)

import Arkham.Classes.GameLogger
import Arkham.Id
import Arkham.Name
import Data.Aeson.TH
import Data.Char (isUpper, toLower)

data ScenarioLogKey
  = HadADrink (Labeled InvestigatorId)
  | -- | The House Always Wins
    Cheated
  | FoundAStrangeDoll
  | -- | Curse of the Rougarou
    FoundAnAncientBindingStone
  | -- | The Essex County Exress
    StolenAPassengersLuggage
  | -- | Curtain Call
    StoleFromTheBoxOffice
  | InterviewedConstance
  | InterviewedJordan
  | InterviewedHaruko
  | InterviewedSebastien
  | -- | The Last King
    InterviewedAshleigh
  | SetAFireInTheKitchen
  | IncitedAFightAmongstThePatients
  | DistractedTheGuards
  | ReleasedADangerousPatient
  | KnowTheGuardsPatrols
  | RecalledTheWayOut
  | -- | The Unspeakable Oath
    YouTookTheKeysByForce
  | -- | The Pallid Mask
    YouOpenedASecretPassageway
  | FoundAGuide
  | -- | Black Stars Rise
    FoundTheTowerKey
  | -- | DimCarcosa
    KnowTheSecret
  | IchtachaIsLeadingTheWay
  | -- | The Untamed Wilds
    YouFoughtWithIchtaca
  | YouListenedToIchtacasTale
  | IchtacaLeftWithoutYou
  | IchtacasPrey (Labeled EnemyId)
  | -- | Threads of Fate
    IchtacasDestination (Labeled LocationId)
  | FoundTheProcess
  | DissectedAnOrgan
  | InterviewedASubject
  | RealizedWhatYearItIs
  | -- | The City of Archives
    ActivatedTheDevice
  | -- | The Depths of Yoth
    CollectedAStrangeLiquid
  | MeddledWithThePast (Labeled InvestigatorId)
  | -- | The Search for Kadath
    KnowWhatHappenedToIb
  | ObtainedSuppliesFromBaharna
  | BeseechedTheKing
  | -- | A Thousand Shapes of Horror
    FoundACrackedMirror
  | StudiedADesecratedPortrait
  | NoticedTheMissingBones
  | RecoveredAStrangeKey
  | -- | Where the Gods Dwell
    ManeuveredThePriestCloser
  | StunnedThePriest
  | -- | Murder at the Excelsior Hotel
    CleanedUpTheBlood
  | HidTheBody
  | TidiedUpTheRoom
  | ThePoliceDon'tBelieveYou
  | ThePoliceAreOnYourSide
  | -- Investigator Cards
    YouOweBiancaResources (Labeled InvestigatorId) Int
  deriving stock (Eq, Show, Ord, Data)

data ScenarioCountKey
  = CurrentDepth
  | SignOfTheGods
  | Distortion
  deriving stock (Eq, Show, Ord, Data)

instance ToGameLoggerFormat ScenarioLogKey where
  format = \case
    YouOweBiancaResources (Labeled name iid) n ->
      "{investigator:\""
        <> display name
        <> "\":"
        <> tshow iid
        <> "} owes Bianca "
        <> tshow n
        <> " resources"
    IchtacasPrey (Labeled name eid) -> "{enemy:\"" <> display name <> "\":" <> tshow eid <> "} is Ichtaca's Prey"
    IchtacasDestination (Labeled name lid) -> "{location:\"" <> display name <> "\":" <> tshow lid <> "} is Ichtaca's Destination"
    HadADrink (Labeled name iid) -> "{investigator:\"" <> display name <> "\":" <> tshow iid <> "} had a drink"
    MeddledWithThePast (Labeled name iid) -> "{investigator:\"" <> display name <> "\":" <> tshow iid <> "} meddled with the past"
    other -> pack . go $ show other
   where
    go :: String -> String
    go [] = []
    go (x : xs) = toLower x : go' xs

    go' :: String -> String
    go' [] = []
    go' (x : xs) | isUpper x = ' ' : toLower x : go' xs
    go' (x : xs) = x : go' xs

$(deriveJSON defaultOptions ''ScenarioLogKey)
$(deriveJSON defaultOptions ''ScenarioCountKey)

instance ToJSONKey ScenarioLogKey
instance FromJSONKey ScenarioLogKey
instance ToJSONKey ScenarioCountKey
instance FromJSONKey ScenarioCountKey
