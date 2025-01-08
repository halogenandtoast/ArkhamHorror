module Arkham.Campaign.Campaigns.ThePathToCarcosa (thePathToCarcosa) where

import Arkham.Campaign.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.CampaignSteps
import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Xp
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log

newtype ThePathToCarcosa = ThePathToCarcosa CampaignAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

instance IsCampaign ThePathToCarcosa where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just CurtainCall
    CurtainCall -> Just (UpgradeDeckStep TheLastKing)
    TheLastKing -> Just (UpgradeDeckStep EchoesOfThePast)
    InterludeStep 1 _ -> Just (UpgradeDeckStep EchoesOfThePast)
    EchoesOfThePast -> Just (UpgradeDeckStep TheUnspeakableOath)
    TheUnspeakableOath -> Just (UpgradeDeckStep APhantomOfTruth)
    InterludeStep 2 _ -> Just (UpgradeDeckStep APhantomOfTruth)
    APhantomOfTruth -> Just (UpgradeDeckStep ThePallidMask)
    ThePallidMask -> Just (UpgradeDeckStep BlackStarsRise)
    BlackStarsRise -> Just (UpgradeDeckStep DimCarcosa)
    DimCarcosa -> Just EpilogueStep
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

thePathToCarcosa :: Difficulty -> ThePathToCarcosa
thePathToCarcosa difficulty =
  campaign
    ThePathToCarcosa
    (CampaignId "03")
    "The Path to Carcosa"
    difficulty
    (chaosBagContents difficulty)

instance RunMessage ThePathToCarcosa where
  runMessage msg c = runQueueT $ case msg of
    CampaignStep PrologueStep -> do
      story prologue
      whenAny (InvestigatorWithTitle "Lola Hayes") $ story lolaPrologue
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 1 _) -> do
      vipsSlain <- getRecordSet VIPsSlain
      let unslain =
            recordedCardCodes
              $ filter (`notElem` vipsSlain)
              $ map
                recorded
                [ Enemies.constanceDumaine.cardCode
                , Enemies.jordanPerry.cardCode
                , Enemies.ishimaruHaruko.cardCode
                , Enemies.sebastienMoreau.cardCode
                , Enemies.ashleighClarke.cardCode
                ]
      removeAllChaosTokens Cultist
      removeAllChaosTokens Tablet
      removeAllChaosTokens ElderThing
      lead <- getLead
      chooseOneM lead do
        labeled
          "Things seem to have calmed down. Perhaps we should go back inside and investigate further."
          do
            story lunacysReward1
            record YouIntrudedOnASecretMeeting
            markDoubt
            addChaosToken ElderThing
            addChaosToken ElderThing
            nextCampaignStep
        labeled "I don't trust this place one bit. Let's block the door and get the hell out of here!" do
          story lunacysReward2
          record YouFledTheDinnerParty
          addChaosToken Tablet
          addChaosToken Tablet
        labeled
          "If these people are allowed to live, these horrors will only repeat themselves. We have to put an end to this. We have to kill them."
          do
            story lunacysReward3
            record YouSlayedTheMonstersAtTheDinnerParty
            unless (null unslain) do
              recordSetInsert VIPsSlain unslain
            markConviction
            addChaosToken Cultist
            addChaosToken Cultist

      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 2 mInterludeKey) -> do
      case mInterludeKey of
        Nothing -> error "Missing key from The Unspeakable Oath"
        Just DanielSurvived -> do
          story danielSurvived
          interludeXpAll (WithBonus "Gain insight into the machinations of the Tattered King." 2)
        Just DanielDidNotSurvive -> story danielDidNotSurvive
        Just DanielWasPossessed -> story danielWasPossessed
        Just _ -> error "Invalid key for The Unspeakable Oath"

      lead <- getLead
      chooseOneM lead do
        labeled
          "Possession? Oaths? There must be another explanation for all of this. Proceed to Ignore the Warning."
          do
            story ignoreTheWarning
            record YouIgnoredDanielsWarning
            markDoubtN 2
        labeled
          "We must heed Daniel’s warning. We must not speak the name of the King in Yellow. Proceed to Heed the Warning."
          do
            story headTheWarning
            record YouHeadedDanielsWarning
            markConvictionN 2
            interludeXpAll (WithBonus "Gain insight into the machinations of the Tattered King." 1)

      nextCampaignStep
      pure c
    CampaignStep EpilogueStep -> do
      possessed <- getRecordSet Possessed
      let
        investigators = flip mapMaybe possessed $ \case
          SomeRecorded RecordableCardCode (Recorded cardCode) -> Just (InvestigatorId cardCode)
          _ -> Nothing
      unless (null investigators) $ storyOnly investigators epilogue
      gameOver
      pure c
    EnemyDefeated _ cardId _ _ -> do
      card <- getCard cardId
      when (card `cardMatch` cardIs Enemies.theManInThePallidMask) do
        n <- getRecordCount ChasingTheStranger
        recordCount ChasingTheStranger (n + 1)
      pure c
    _ -> lift $ defaultCampaignRunner msg c
