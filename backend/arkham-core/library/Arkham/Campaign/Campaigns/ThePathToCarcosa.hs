module Arkham.Campaign.Campaigns.ThePathToCarcosa
  ( ThePathToCarcosa(..)
  , thePathToCarcosa
  ) where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.CampaignStep
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Matcher hiding ( EnemyDefeated )
import Arkham.Message
import Arkham.Token

newtype ThePathToCarcosa = ThePathToCarcosa CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

thePathToCarcosa :: Difficulty -> ThePathToCarcosa
thePathToCarcosa difficulty = campaign
  ThePathToCarcosa
  (CampaignId "03")
  "The Path to Carcosa"
  difficulty
  (chaosBagContents difficulty)

instance RunMessage ThePathToCarcosa where
  runMessage msg c@(ThePathToCarcosa a) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- allInvestigatorIds
      lolaHayesChosen <- isJust
        <$> selectOne (InvestigatorWithTitle "Lola Hayes")
      pushAll
        $ [story investigatorIds prologue]
        <> [ story investigatorIds lolaPrologue | lolaHayesChosen ]
        <> [NextCampaignStep Nothing]
      pure c
    CampaignStep (Just (InterludeStep 1 _)) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- allInvestigatorIds
      doubt <- getRecordCount Doubt
      conviction <- getRecordCount Conviction
      push $ chooseOne
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
      pure c
    CampaignStep (Just (InterludeStep 2 mInterludeKey)) -> do
      investigatorIds <- allInvestigatorIds
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
        Just _ -> error "Invalid key for The Unspeakable Oath"
    CampaignStep (Just EpilogueStep) -> do
      possessed <- getRecordSet Possessed
      let
        investigatorIds = flip mapMaybe possessed $ \case
          Recorded cardCode -> Just (InvestigatorId cardCode)
          _ -> Nothing
      pushAll
        $ [ story investigatorIds epilogue | notNull investigatorIds ]
        <> [EndOfGame Nothing]
      pure c
    NextCampaignStep _ -> do
      let step = nextStep a
      push (CampaignStep step)
      pure
        . ThePathToCarcosa
        $ a
        & (stepL .~ step)
        & (completedStepsL %~ completeStep (campaignStep a))
    EnemyDefeated _ cardCode _ _
      | cardCode == toCardCode Enemies.theManInThePallidMask -> do
        n <- getRecordCount ChasingTheStranger
        c <$ push (RecordCount ChasingTheStranger (n + 1))
    _ -> ThePathToCarcosa <$> runMessage msg a
