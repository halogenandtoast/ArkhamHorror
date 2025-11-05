{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.CampaignStep where

import Arkham.Id
import Arkham.Prelude
import Data.Aeson.TH
import GHC.Records

data Continuation = Continuation
  { nextStep :: CampaignStep
  , canUpgradeDecks :: Bool
  , chooseSideStory :: Bool
  }
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass ToJSON

data CampaignStep
  = PrologueStep
  | PrologueStepPart Int
  | ScenarioStep ScenarioId
  | ScenarioStepPart ScenarioId Int
  | InterludeStep Int (Maybe InterludeKey)
  | InterludeStepPart Int (Maybe InterludeKey) Int
  | UpgradeDeckStep CampaignStep
  | EpilogueStep
  | EpilogueStepPart Int
  | InvestigatorCampaignStep InvestigatorId CampaignStep
  | ResupplyPoint
  | CheckpointStep Int
  | CampaignSpecificStep Text (Maybe Text)
  | ContinueCampaignStep Continuation
  | StandaloneScenarioStep ScenarioId CampaignStep
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass ToJSON

continue :: CampaignStep -> Maybe CampaignStep
continue ns = Just $ ContinueCampaignStep $ Continuation ns True False

continueNoUpgrade :: CampaignStep -> Maybe CampaignStep
continueNoUpgrade ns = Just $ ContinueCampaignStep $ Continuation ns False False

instance HasField "normalize" CampaignStep CampaignStep where
  getField = normalizedCampaignStep

instance HasField "unwrap" CampaignStep CampaignStep where
  getField = \case
    ContinueCampaignStep cont -> cont.nextStep
    other -> other

instance HasField "isPrologue" CampaignStep Bool where
  getField = \case
    PrologueStep -> True
    PrologueStepPart _ -> True
    ContinueCampaignStep cont -> getField @"isPrologue" cont.nextStep
    _ -> False

defaultNextStep :: CampaignStep -> Maybe CampaignStep
defaultNextStep = \case
  UpgradeDeckStep nextStep' -> Just nextStep'
  ContinueCampaignStep cont -> Just cont.nextStep
  StandaloneScenarioStep _ nextStep' -> Just nextStep'
  _ -> Nothing

data InterludeKey
  = DanielSurvived
  | DanielWasPossessed
  | DanielDidNotSurvive
  | TheCustodianWasUnderControl
  | -- The Circle Undone
    ThePriceOfProgress4
  | ThePriceOfProgress5
  | ThePriceOfProgress6
  | -- The Innsmouth Conspiracy
    HasPurpleKey
  | HasWhiteKey
  | HasBlackKey
  | HasPurpleAndWhiteKeys
  | HasPurpleAndBlackKeys
  | HasWhiteAndBlackKeys
  | HasPurpleWhiteAndBlackKeys
  | HasNoKeys
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

normalizedCampaignStep :: CampaignStep -> CampaignStep
normalizedCampaignStep = \case
  PrologueStep -> PrologueStep
  PrologueStepPart _ -> PrologueStep
  ScenarioStep sid -> ScenarioStep sid
  ScenarioStepPart sid _ -> ScenarioStep sid
  InterludeStep n _ -> InterludeStep n Nothing
  InterludeStepPart n _ _ -> InterludeStep n Nothing
  UpgradeDeckStep c -> normalizedCampaignStep c
  EpilogueStep -> EpilogueStep
  EpilogueStepPart _ -> EpilogueStep
  InvestigatorCampaignStep _ c -> normalizedCampaignStep c
  ResupplyPoint -> ResupplyPoint
  CheckpointStep n -> CheckpointStep n
  CampaignSpecificStep t ms -> CampaignSpecificStep t ms
  ContinueCampaignStep c -> ContinueCampaignStep c
  StandaloneScenarioStep sid c -> StandaloneScenarioStep sid c

[d|
  deriving anyclass instance FromJSON Continuation

  instance FromJSON CampaignStep where
    parseJSON = withObject "CampaignStep" \o -> do
      tag <- o .:? "tag"
      case (tag :: Maybe Text) of
        Just "CampaignSpecificStep" -> do
          contents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
          pure $ case contents of
            Right (t, ms) -> CampaignSpecificStep t ms
            Left t -> CampaignSpecificStep t Nothing
        Just "ContinueCampaignStep" -> do
          contents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
          case contents of
            Left nextStep -> pure $ ContinueCampaignStep $ Continuation nextStep True False
            Right inner -> do
              nextStep <- inner .:? "nextStep"
              case nextStep of
                Just ns -> do
                  canUpgrade <- inner .: "canUpgradeDecks"
                  chooseSideStory <- inner .:? "chooseSideStory" .!= False
                  pure $ ContinueCampaignStep $ Continuation ns canUpgrade chooseSideStory
                Nothing -> do
                  nStep <- parseJSON (Object inner)
                  pure $ ContinueCampaignStep $ Continuation nStep True False
        _ -> $(mkParseJSON defaultOptions ''CampaignStep) (Object o)
  |]
