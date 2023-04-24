{-# LANGUAGE TemplateHaskell #-}
module Arkham.ScenarioLogKey where

import Arkham.Prelude hiding (toLower)

import Arkham.Id
import Arkham.Name
import Arkham.Classes.GameLogger
import Data.Aeson.TH
import Data.Char (isUpper, toLower)

data ScenarioLogKey
  = HadADrink (Labeled InvestigatorId)
  | Cheated
  -- ^ The House Always Wins
  | FoundAStrangeDoll
  | FoundAnAncientBindingStone
  -- ^ Curse of the Rougarou
  | StolenAPassengersLuggage
  -- ^ The Essex County Exress
  | StoleFromTheBoxOffice
  -- ^ Curtain Call
  | InterviewedConstance
  | InterviewedJordan
  | InterviewedHaruko
  | InterviewedSebastien
  | InterviewedAshleigh
  -- ^ The Last King
  | SetAFireInTheKitchen
  | IncitedAFightAmongstThePatients
  | DistractedTheGuards
  | ReleasedADangerousPatient
  | KnowTheGuardsPatrols
  | RecalledTheWayOut
  | YouTookTheKeysByForce
  -- ^ The Unspeakable Oath
  | YouOpenedASecretPassageway
  -- ^ The Pallid Mask
  | FoundAGuide
  | FoundTheTowerKey
  -- ^ Black Stars Rise
  | KnowTheSecret
  -- ^ DimCarcosa
  | IchtachaIsLeadingTheWay
  | YouFoughtWithIchtaca
  -- ^ The Untamed Wilds
  | YouListenedToIchtacasTale
  | IchtacaLeftWithoutYou
  | IchtacasPrey (Labeled EnemyId)
  | IchtacasDestination (Labeled LocationId)
  -- ^ Threads of Fate
  | FoundTheProcess
  | DissectedAnOrgan
  | InterviewedASubject
  | RealizedWhatYearItIs
  | ActivatedTheDevice
  -- ^ The City of Archives
  | CollectedAStrangeLiquid
  -- ^ The Depths of Yoth
  | MeddledWithThePast (Labeled InvestigatorId)
  deriving stock (Eq, Show, Ord)

data ScenarioCountKey = CurrentDepth | PlaceholderCountKey
  deriving stock (Eq, Show, Ord)

instance ToGameLoggerFormat ScenarioLogKey where
  format = \case
    IchtacasPrey (Labeled name eid) -> "{enemy:\"" <> display name <> "\":" <> tshow eid <> "} is Ichtaca's Prey"
    IchtacasDestination (Labeled name lid) -> "{location:\"" <> display name <> "\":" <> tshow lid <> "} is Ichtaca's Destination"
    HadADrink (Labeled name iid) -> "{investigator:\"" <> display name <> "\":" <> tshow iid <> "} had a drink"
    MeddledWithThePast (Labeled name iid) -> "{investigator:\"" <> display name <> "\":" <> tshow iid <> "} meddled with the past"
    other -> pack . go $ show other
    where
      go :: String -> String
      go [] = []
      go (x:xs) = toLower x : go' xs

      go' :: String -> String
      go' [] = []
      go' (x:xs) | isUpper x = ' ' : toLower x : go' xs
      go' (x:xs) = x : go' xs

$(deriveJSON defaultOptions ''ScenarioLogKey)
$(deriveJSON defaultOptions ''ScenarioCountKey)

instance ToJSONKey ScenarioLogKey
instance FromJSONKey ScenarioLogKey
instance ToJSONKey ScenarioCountKey
instance FromJSONKey ScenarioCountKey
