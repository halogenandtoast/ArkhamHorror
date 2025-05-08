module Arkham.Campaign.Campaigns.ThePathToCarcosa (thePathToCarcosa, ThePathToCarcosa (..)) where

import Arkham.Campaign.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.CampaignSteps
import Arkham.Campaigns.ThePathToCarcosa.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.FlavorText
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
  runMessage msg c = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> scope "prologue" do
      flavor do
        setTitle "yellowSign.title"
        p "yellowSign.body"

      isLola <- selectAny $ InvestigatorWithTitle "Lola Hayes"
      flavor do
        setTitle "title"
        p "body"
        p.right.validate isLola "ifLola"
        p.right.validate (not isLola) "otherwise"
      when isLola $ flavor $ p "lola" >> p.right "proceed"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 1 _) -> scope "interlude1" do
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

      let interlude k = storyBuild $ setTitle "title" >> p k

      storyWithChooseOneM' (setTitle "title" >> p "body") do
        labeled' "chooseLunacysReward1" do
          interlude "lunacysReward1"
          record YouIntrudedOnASecretMeeting
          markDoubt
          addChaosToken ElderThing
          addChaosToken ElderThing
        labeled' "chooseLunacysReward2" do
          interlude "lunacysReward2"
          record YouFledTheDinnerParty
          addChaosToken Tablet
          addChaosToken Tablet
        labeled' "chooseLunacysReward3" do
          interlude "lunacysReward3"
          record YouSlayedTheMonstersAtTheDinnerParty
          unless (null unslain) $ recordSetInsert VIPsSlain unslain
          markConviction
          addChaosToken Cultist
          addChaosToken Cultist

      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 2 mInterludeKey) -> scope "interlude2" do
      let interlude k = storyBuild $ setTitle "title" >> p k
      let
        handleWarning = do
          labeled' "ignoreTheWarning" do
            interlude "ignoreTheWarning"
            record YouIgnoredDanielsWarning
            markDoubtN 2
          labeled' "heedTheWarning" do
            interlude "heedTheWarning"
            record YouHeadedDanielsWarning
            markConvictionN 2
            interludeXpAll (toBonus "bonus" 1)
      case mInterludeKey of
        Nothing -> error "Missing key from The Unspeakable Oath"
        Just DanielSurvived -> do
          storyWithChooseOneM' (setTitle "title" >> p "danielSurvived") handleWarning
          interludeXpAll (toBonus "bonus" 2)
        Just DanielDidNotSurvive ->
          storyWithChooseOneM' (setTitle "title" >> p "danielDidNotSurvive") handleWarning
        Just DanielWasPossessed ->
          storyWithChooseOneM' (setTitle "title" >> p "danielWasPossessed") handleWarning
        Just _ -> error "Invalid key for The Unspeakable Oath"

      nextCampaignStep
      pure c
    CampaignStep EpilogueStep -> scope "epilogue" do
      possessed <- getRecordSet Possessed
      let
        investigators = flip mapMaybe possessed $ \case
          SomeRecorded RecordableCardCode (Recorded cardCode) -> Just (InvestigatorId cardCode)
          _ -> Nothing
      unless (null investigators) $ storyOnlyBuild investigators do
        setTitle "title"
        p "body"
      gameOver
      pure c
    EnemyDefeated _ cardId _ _ -> do
      card <- getCard cardId
      when (card `cardMatch` cardIs Enemies.theManInThePallidMask) do
        n <- getRecordCount ChasingTheStranger
        recordCount ChasingTheStranger (n + 1)
      pure c
    _ -> lift $ defaultCampaignRunner msg c
