module Arkham.Campaign.Campaigns.TheInnsmouthConspiracy (
  TheInnsmouthConspiracy (..),
  theInnsmouthConspiracy,
) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Import
import Arkham.ChaosToken
import Arkham.Helpers.Campaign (getOwner, withOwner)
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Source

newtype TheInnsmouthConspiracy = TheInnsmouthConspiracy CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theInnsmouthConspiracy :: Difficulty -> TheInnsmouthConspiracy
theInnsmouthConspiracy difficulty =
  campaign
    TheInnsmouthConspiracy
    (CampaignId "07")
    "The Innsmouth Conspiracy"
    difficulty
    (chaosBagContents difficulty)

instance IsCampaign TheInnsmouthConspiracy where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just ThePitOfDespair
    ThePitOfDespair -> Just (InterludeStep 1 Nothing)
    InterludeStep 1 _ -> Just TheVanishingOfElinaHarper
    TheVanishingOfElinaHarper -> Just (InterludeStep 2 Nothing)
    InterludeStep 2 _ -> Just (UpgradeDeckStep InTooDeep)
    InTooDeep -> Just DevilReef
    -- Devil Reef must choose interlude options
    InterludeStep 3 _ -> Just (UpgradeDeckStep HorrorInHighGear)
    HorrorInHighGear -> Just ALightInTheFog
    ALightInTheFog -> Just TheLairOfDagon
    TheLairOfDagon -> Just (InterludeStep 4 Nothing)
    InterludeStep 4 _ -> Just (UpgradeDeckStep IntoTheMaelstrom)
    IntoTheMaelstrom -> Just EpilogueStep
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

instance RunMessage TheInnsmouthConspiracy where
  runMessage msg c@(TheInnsmouthConspiracy _attrs) = runQueueT $ campaignI18n $ case msg of
    StartCampaign -> do
      recordSetInsert PossibleSuspects
        $ map toJSON [BrianBurnham, BarnabasMarsh, OtheraGilman, ZadokAllen, JoyceLittle, RobertFriendly]
      recordSetInsert PossibleHideouts
        $ map
          toJSON
          [ InnsmouthJail
          , ShorewardSlums
          , SawboneAlley
          , TheHouseOnWaterStreet
          , EsotericOrderOfDagon
          , NewChurchGreen
          ]
      lift $ defaultCampaignRunner msg c
    CampaignStep PrologueStep -> do
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 1 _) -> scope "interlude1" do
      story $ i18nWithTitle "part1"
      memoriesRecovered <- getRecordSet MemoriesRecovered
      when (recorded AMeetingWithThomasDawson `elem` memoriesRecovered) $ do
        story $ i18nWithTitle "aMeetingWithThomasDawson"
        eachInvestigator $ \iid -> gainXp iid CampaignSource (ikey "xp.aMeetingWithThomasDawson") 1
      when (null memoriesRecovered) $ do
        story $ i18nWithTitle "noMemoriesRecovered"
      when (recorded ABattleWithAHorrifyingDevil `elem` memoriesRecovered) $ do
        story $ i18nWithTitle "aBattleWithAHorrifyingDevil"
        eachInvestigator $ \iid -> gainXp iid CampaignSource (ikey "xp.aBattleWithAHorrifyingDevil") 1
      when (recorded ADecisionToStickTogether `elem` memoriesRecovered) $ do
        story $ i18nWithTitle "aDecisionToStickTogether"
        eachInvestigator $ \iid -> gainXp iid CampaignSource (ikey "xp.aDecisionToStickTogether") 1
      when (recorded AnEncounterWithASecretCult `elem` memoriesRecovered) $ do
        story $ i18nWithTitle "anEncounterWithASecretCult"
        eachInvestigator $ \iid -> gainXp iid CampaignSource (ikey "xp.anEncounterWithASecretCult") 1
      story $ i18nWithTitle "part2"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 2 _) -> scope "interlude2" do
      story $ i18nWithTitle "theSyzygy1"
      whenHasRecord TheMissionFailed $ story $ i18nWithTitle "theSyzygy2"
      whenHasRecord TheMissionWasSuccessful do
        story $ i18nWithTitle "theSyzygy3"
        investigators <- allInvestigators
        addCampaignCardToDeckChoice investigators Assets.elinaHarperKnowsTooMuch
      story $ i18nWithTitle "theSyzygy4"
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 3 (Just keysFound)) -> scope "interlude3" do
      story $ i18nWithTitle "beneathTheWaves1"

      when
        ( keysFound
            `elem` [HasPurpleKey, HasPurpleAndWhiteKeys, HasPurpleAndBlackKeys, HasPurpleWhiteAndBlackKeys]
        )
        do
          story $ i18n "purpleKey"
          interludeXpAll (toBonus "purpleKey" 2)
          addChaosToken Cultist
          record TheIdolWasBroughtToTheLighthouse

      when
        ( keysFound
            `elem` [HasWhiteKey, HasPurpleAndWhiteKeys, HasWhiteAndBlackKeys, HasPurpleWhiteAndBlackKeys]
        )
        do
          story $ i18n "whiteKey"
          interludeXpAll (toBonus "whiteKey" 2)
          addChaosToken Tablet
          record TheMantleWasBroughtToTheLighthouse

      when
        ( keysFound
            `elem` [HasBlackKey, HasPurpleAndBlackKeys, HasWhiteAndBlackKeys, HasPurpleWhiteAndBlackKeys]
        )
        do
          story $ i18n "blackKey"
          interludeXpAll (toBonus "blackKey" 2)
          addChaosToken ElderThing
          record TheHeaddressWasBroughtToTheLighthouse

      story $ i18n "beneathTheWaves2"

      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 4 _) -> scope "interlude4" do
      story $ i18nWithTitle "hiddenTruths"

      terrorDead <- getHasRecord TheTerrorOfDevilReefIsDead
      lifecycleKnown <- hasMemory TheLifecycleOfADeepOne

      when (terrorDead && lifecycleKnown) do
        story $ i18n "guardianDispatched"
        record TheGuardianOfYhanthleiIsDispatched

      gatekeeperDefeated <- getHasRecord TheGatekeeperHasBeenDefeated
      someRelic <-
        orM
          $ map
            (fmap isJust . getOwner)
            [Assets.awakenedMantle, Assets.headdressOfYhaNthlei, Assets.wavewornIdol]

      when (gatekeeperDefeated && someRelic) do
        story $ i18n "rightfulKeeper"
        record TheGatewayToYhanthleiRecognizesYouAsTheRightfulKeeper

      removeCampaignCard Assets.thomasDawsonSoldierInANewWar
      withOwner Assets.elinaHarperKnowsTooMuch \iid -> do
        chooseOneM iid do
          labeled "Add Elina Harper to your deck" do
            addCampaignCardToDeck iid Assets.elinaHarperKnowsTooMuch
          labeled "Do not add Elina Harper to your deck" do
            removeCampaignCard Assets.elinaHarperKnowsTooMuch

      nextCampaignStep
      pure c
    CampaignStep EpilogueStep -> scope "epilogue" do
      story $ i18nWithTitle "epilogue1"
      memories <- getRecordSet MemoriesRecovered
      if all
        ((`elem` memories) . recorded)
        [ AMeetingWithThomasDawson
        , ABattleWithAHorrifyingDevil
        , ADecisionToStickTogether
        , AnEncounterWithASecretCult
        , ADealWithJoeSargent
        , AFollowedLead
        , AnIntervention
        , AJailbreak
        , DiscoveryOfAStrangeIdol
        , DiscoveryOfAnUnholyMantle
        , DiscoveryOfAMysticalRelic
        , AConversationWithMrMoore
        , TheLifecycleOfADeepOne
        , AStingingBetrayal
        ]
        then do
          record TheHorribleTruth
          story $ i18nWithTitle "flashback15"
        else story $ i18nWithTitle "epilogue2"
      gameOver

      pure c
    _ -> lift $ defaultCampaignRunner msg c
