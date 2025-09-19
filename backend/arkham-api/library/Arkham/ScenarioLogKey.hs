{-# LANGUAGE TemplateHaskell #-}

module Arkham.ScenarioLogKey where

import Arkham.Card.CardCode
import Arkham.Classes.GameLogger
import Arkham.Id
import Arkham.Name
import Arkham.Prelude hiding (toLower)
import Control.Monad.Fail
import Data.Aeson.TH
import Data.Char (isUpper, toLower)

data ScenarioLogKey
  = HadADrink (Labeled InvestigatorId)
  | -- | The House Always Wins
    Cheated (Labeled InvestigatorId)
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
  | IchtacasPrey (Labeled EnemyId `With` Envelope "cardCode" CardCode)
  | -- | Threads of Fate
    IchtacasDestination (Labeled LocationId `With` Envelope "cardCode" CardCode)
  | -- | The City of Archives
    FoundTheProcess
  | DissectedAnOrgan
  | InterviewedASubject
  | RealizedWhatYearItIs
  | ActivatedTheDevice
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
  | -- | The Lair of Dagon
    UnlockedTheEntranceToTheCaves
  | UnlockedTheThirdFloor
  | UnlockedTheFinalDepths
  | -- | City of the ElderThings
    TheTeamStudiedTheHistoryOfTheElderThings
  | TheTeamDiscernedTheOriginOfTheShoggoths
  | TheTeamDiscoveredAHiddenPower
  | -- | Return to the City of Archives
    ReadAboutEarth
  | SawAFamiliarSpecimen
  | -- | Murder at the Excelsior Hotel
    CleanedUpTheBlood
  | HidTheBody
  | TidiedUpTheRoom
  | ThePoliceDon'tBelieveYou
  | ThePoliceAreOnYourSide
  | -- | Film Fatale
    TheInvestigatorsMadeTheirCallTime
  | -- Investigator Cards
    YouOweBiancaResources (Labeled InvestigatorId) Int
  deriving stock (Eq, Show, Ord, Data)

data ScenarioCountKey
  = CurrentDepth
  | SignOfTheGods
  | Distortion
  | Barriers LocationId LocationId
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
    IchtacasPrey (Labeled name eid `With` Envelope cardCode) ->
      "{enemy:\"" <> display name <> "\":" <> tshow eid <> ":" <> tshow cardCode <> "} is Ichtaca's Prey"
    IchtacasDestination (Labeled name lid `With` Envelope cardCode) ->
      "{location:\""
        <> display name
        <> "\":"
        <> tshow lid
        <> ":"
        <> tshow cardCode
        <> "} is Ichtaca's Destination"
    HadADrink (Labeled name iid) -> "{investigator:\"" <> display name <> "\":" <> tshow iid <> "} had a drink"
    Cheated (Labeled name iid) -> "{investigator:\"" <> display name <> "\":" <> tshow iid <> "} cheated"
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

$(deriveToJSON defaultOptions ''ScenarioLogKey)

instance FromJSON ScenarioLogKey where
  parseJSON = withObject "ScenarioLogKey" \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "IchtacasDestination" -> do
        econtents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
        pure $ case econtents of
          Right contents -> IchtacasDestination contents
          Left contents -> IchtacasDestination $ contents `With` Envelope @"cardCode" "01130"
      _ -> $(mkParseJSON defaultOptions ''ScenarioLogKey) (Object o)

$(deriveToJSON defaultOptions ''ScenarioCountKey)

instance FromJSON ScenarioCountKey where
  parseJSON = \case
    String "CurrentDepth" -> pure CurrentDepth
    String "SignOfTheGods" -> pure SignOfTheGods
    String "Distortion" -> pure Distortion
    Object o -> do
      tag :: Text <- o .: "tag"
      case tag of
        "Barriers" -> do
          (x, y) <- o .: "contents"
          pure $ Barriers x y
        "CurrentDepth" -> pure CurrentDepth
        "SignOfTheGods" -> pure SignOfTheGods
        "Distortion" -> pure Distortion
        _ -> fail "Unknown tag"
    _ -> fail "Expected String or Object"

instance ToJSONKey ScenarioLogKey
instance FromJSONKey ScenarioLogKey
instance ToJSONKey ScenarioCountKey
instance FromJSONKey ScenarioCountKey
