module Arkham.Types.Campaign.Campaigns.ThePathToCarcosa
  ( ThePathToCarcosa(..)
  , thePathToCarcosa
  ) where

import Arkham.Prelude

import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignId
import Arkham.Types.CampaignLogKey
import Arkham.Types.CampaignStep
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher hiding (EnemyDefeated)
import Arkham.Types.Message
import Arkham.Types.Token

newtype ThePathToCarcosa = ThePathToCarcosa CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

thePathToCarcosa :: Difficulty -> ThePathToCarcosa
thePathToCarcosa difficulty = ThePathToCarcosa $ baseAttrs
  (CampaignId "03")
  "The Path to Carcosa"
  difficulty
  (chaosBagContents difficulty)

instance CampaignRunner env => RunMessage env ThePathToCarcosa where
  runMessage msg c@(ThePathToCarcosa a) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- getInvestigatorIds
      lolaHayesChosen <- isJust
        <$> selectOne (InvestigatorWithTitle "Lola Hayes")
      c <$ pushAll
        ([story investigatorIds prologue]
        <> [ story investigatorIds lolaPrologue | lolaHayesChosen ]
        <> [NextCampaignStep Nothing]
        )
    CampaignStep (Just (InterludeStep 1 _)) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      doubt <- getRecordCount Doubt
      conviction <- getRecordCount Conviction
      c <$ push
        (chooseOne
          leadInvestigatorId
          [ Label
            "Things seem to have calmed down. Perhaps we should go back inside and investigate further."
            [ story investigatorIds lunacysReward1
            , Record YouIntrudedOnASecretMeeting
            , RecordCount Doubt (doubt + 1)
            , RemoveAllTokens Cultist
            , RemoveAllTokens Tablet
            , RemoveAllTokens ElderThing
            , AddToken ElderThing
            , AddToken ElderThing
            , NextCampaignStep Nothing
            ]
          , Label
            "I don't trust this place one bit. Letbs block the door and get the hell out of here!"
            [ story investigatorIds lunacysReward2
            , Record YouFledTheDinnerParty
            , RemoveAllTokens Cultist
            , RemoveAllTokens Tablet
            , RemoveAllTokens ElderThing
            , AddToken Tablet
            , AddToken Tablet
            , NextCampaignStep Nothing
            ]
          , Label
            "If these people are allowed to live, these horrors will only repeat themselves. We have to put an end to this. We have to kill them."
            [ story investigatorIds lunacysReward3
            , Record YouSlayedTheMonstersAtTheDinnerParty
            , RecordCount Conviction (conviction + 1)
            , RemoveAllTokens Cultist
            , RemoveAllTokens Tablet
            , RemoveAllTokens ElderThing
            , AddToken Cultist
            , AddToken Cultist
            , NextCampaignStep Nothing
            ]
          ]
        )
    CampaignStep (Just (InterludeStep 2 mInterludeKey)) -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      conviction <- getRecordCount Conviction
      doubt <- getRecordCount Doubt
      let
        respondToWarning = chooseOne
          leadInvestigatorId
          [ Label
            "Possession? Oaths? There must be another explanation for all of this. Proceed to Ignore the Warning."
            [ story investigatorIds ignoreTheWarning
            , Record YouIgnoredDanielsWarning
            , RecordCount Doubt (doubt + 2)
            , NextCampaignStep Nothing
            ]
          , Label
            "We must heed Daniel’s warning. We must not speak the name of the King in Yellow. Proceed to Heed the Warning."
            ([ story investigatorIds headTheWarning
             , Record YouHeadedDanielsWarning
             , RecordCount Conviction (conviction + 2)
             ]
            <> map (`GainXP` 1) investigatorIds
            <> [NextCampaignStep Nothing]
            )
          ]
      case mInterludeKey of
        Nothing -> error "Missing key from The Unspeakable Oath"
        Just DanielSurvived -> do
          pushAll
            $ [story investigatorIds danielSurvived]
            <> map (`GainXP` 2) investigatorIds
            <> [respondToWarning]
          pure c
        Just DanielDidNotSurvive -> do
          c <$ pushAll
            [story investigatorIds danielDidNotSurvive, respondToWarning]
        Just DanielWasPossessed -> do
          c <$ pushAll
            [story investigatorIds danielWasPossessed, respondToWarning]
        -- Just _ -> error "Invalid key for The Unspeakable Oath"
    NextCampaignStep _ -> do
      let step = nextStep a
      push (CampaignStep step)
      pure
        . ThePathToCarcosa
        $ a
        & (stepL .~ step)
        & (completedStepsL %~ completeStep (campaignStep a))
    EnemyDefeated _ _ _ cardCode _ _
      | cardCode == toCardCode Enemies.theManInThePallidMask -> do
        n <- hasRecordCount ChasingTheStranger (campaignLog a)
        c <$ push (RecordCount ChasingTheStranger (n + 1))
    _ -> ThePathToCarcosa <$> runMessage msg a
