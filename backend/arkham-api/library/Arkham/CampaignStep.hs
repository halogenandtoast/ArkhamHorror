{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.CampaignStep where

import Arkham.Id
import Arkham.Prelude
import Arkham.Scenario.Options
import Data.Aeson.TH
import GHC.Records

data Continuation = Continuation
  { nextStep :: CampaignStep
  , canUpgradeDecks :: Bool
  , chooseSideStory :: Bool
  , lead :: Maybe InvestigatorId
  , canChooseSideStory :: Bool
  }
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass ToJSON

data CampaignStep
  = PrologueStep
  | PrologueStepPart Int
  | ScenarioStep ScenarioId
  | ScenarioStepWithOptions ScenarioId ScenarioOptions
  | InterludeStep Int (Maybe InterludeKey)
  | InterludeStepPart Int (Maybe InterludeKey) Int
  | ChooseDecksStep CampaignStep
  | UpgradeDeckStep CampaignStep
  | EpilogueStep
  | EpilogueStepPart Int
  | InvestigatorCampaignStep InvestigatorId CampaignStep
  | ResupplyPoint
  | CheckpointStep Int
  | CampaignSpecificStep Text (Maybe Text)
  | ContinueCampaignStep Continuation
  | StandaloneScenarioStep ScenarioId CampaignStep
  | StandaloneScenarioStepWithOptions ScenarioId CampaignStep ScenarioOptions
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass ToJSON

continue :: CampaignStep -> Maybe CampaignStep
continue ns = Just $ ContinueCampaignStep $ Continuation ns True False Nothing True

continueEdit :: CampaignStep -> (Continuation -> Continuation) -> Maybe CampaignStep
continueEdit ns f =
  continue ns <&> \case
    ContinueCampaignStep cs -> ContinueCampaignStep $ f cs
    other -> other

noUpgrade :: Continuation -> Continuation
noUpgrade cs = cs {canUpgradeDecks = False}

noSideStory :: Continuation -> Continuation
noSideStory cs = cs {canChooseSideStory = False}

continueNoUpgrade :: CampaignStep -> Maybe CampaignStep
continueNoUpgrade ns = continueEdit ns noUpgrade

continueNoSideScenario :: CampaignStep -> Maybe CampaignStep
continueNoSideScenario ns = continueEdit ns noSideStory

instance HasField "normalize" CampaignStep CampaignStep where
  getField = normalizedCampaignStep

instance HasField "unwrap" CampaignStep CampaignStep where
  getField = \case
    ContinueCampaignStep cont -> cont.nextStep
    other -> other

instance HasField "unwrapScenario" CampaignStep CampaignStep where
  getField = \case
    ScenarioStepWithOptions sid _ -> ScenarioStep sid
    other -> other

instance HasField "scenario" CampaignStep (Maybe ScenarioId) where
  getField = \case
    ScenarioStepWithOptions sid _ -> Just sid
    ScenarioStep sid -> Just sid
    StandaloneScenarioStep sid _ -> Just sid
    StandaloneScenarioStepWithOptions sid _ _ -> Just sid
    _ -> Nothing

instance HasField "isPrologue" CampaignStep Bool where
  getField = \case
    PrologueStep -> True
    PrologueStepPart _ -> True
    ContinueCampaignStep cont -> getField @"isPrologue" cont.nextStep
    _ -> False

defaultNextStep :: CampaignStep -> Maybe CampaignStep
defaultNextStep = \case
  UpgradeDeckStep nextStep' -> Just nextStep'
  ChooseDecksStep nextStep' -> Just nextStep'
  ContinueCampaignStep cont -> Just cont.nextStep
  StandaloneScenarioStep _ nextStep' -> Just nextStep'
  StandaloneScenarioStepWithOptions _ nextStep' _ -> Just nextStep'
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
  ScenarioStepWithOptions sid _ -> ScenarioStep sid
  InterludeStep n _ -> InterludeStep n Nothing
  InterludeStepPart n _ _ -> InterludeStep n Nothing
  UpgradeDeckStep c -> normalizedCampaignStep c
  ChooseDecksStep c -> normalizedCampaignStep c
  EpilogueStep -> EpilogueStep
  EpilogueStepPart _ -> EpilogueStep
  InvestigatorCampaignStep _ c -> normalizedCampaignStep c
  ResupplyPoint -> ResupplyPoint
  CheckpointStep n -> CheckpointStep n
  CampaignSpecificStep t ms -> CampaignSpecificStep t ms
  ContinueCampaignStep c -> ContinueCampaignStep c
  StandaloneScenarioStep sid c -> StandaloneScenarioStep sid c
  StandaloneScenarioStepWithOptions sid c _ -> StandaloneScenarioStep sid c
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
            Left nextStep -> pure $ ContinueCampaignStep $ Continuation nextStep True False Nothing True
            Right inner -> do
              nextStep <- inner .:? "nextStep"
              case nextStep of
                Just ns -> do
                  canUpgrade <- inner .: "canUpgradeDecks"
                  chooseSideStory <- inner .:? "chooseSideStory" .!= False
                  lead <- inner .:? "lead"
                  canChooseSideStory <- inner .:? "canChooseSideStory" .!= True
                  pure $ ContinueCampaignStep $ Continuation ns canUpgrade chooseSideStory lead canChooseSideStory
                Nothing -> do
                  nStep <- parseJSON (Object inner)
                  pure $ ContinueCampaignStep $ Continuation nStep True False Nothing True
        _ -> $(mkParseJSON defaultOptions ''CampaignStep) (Object o)
  |]
