module Arkham.ScenarioLogKey where

import Arkham.Prelude

import Arkham.Id

data ScenarioLogKey
  = HadADrink InvestigatorId
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
  | IchtacasPrey EnemyId
  | IchtacasDestination LocationId
  -- ^ Threads of Fate
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, Hashable, FromJSONKey)
