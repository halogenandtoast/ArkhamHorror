module Arkham.Campaign.Campaigns.TheInnsmouthConspiracy (
  theInnsmouthConspiracy,
  TheInnsmouthConspiracy (..),
  flashback15Memories,
  readEpilogue1,
  recordTheHorribleTruth,
) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Campaigns.TheInnsmouthConspiracy.Achievements (
  runInnsmouthConspiracyAchievements,
 )
import Arkham.Campaign.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.CampaignSteps
import Arkham.Campaigns.TheInnsmouthConspiracy.Import
import Arkham.ChaosToken
import Arkham.Helpers.Campaign (getOwner, withOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Source

newtype TheInnsmouthConspiracy = TheInnsmouthConspiracy CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theInnsmouthConspiracy :: Difficulty -> TheInnsmouthConspiracy
theInnsmouthConspiracy = campaign TheInnsmouthConspiracy (CampaignId "07") "The Innsmouth Conspiracy"

instance IsCampaign TheInnsmouthConspiracy where
  campaignTokens = chaosBagContents
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue ThePitOfDespair
    ThePitOfDespair -> continue $ InterludeStep 1 Nothing
    InterludeStep 1 _ -> continueNoUpgrade TheVanishingOfElinaHarper
    TheVanishingOfElinaHarper -> continue $ InterludeStep 2 Nothing
    InterludeStep 2 _ -> continue InTooDeep
    InTooDeep -> continueNoUpgrade DevilReef
    -- Devil Reef must choose interlude options
    InterludeStep 3 _ -> continue HorrorInHighGear
    HorrorInHighGear -> continue ALightInTheFog
    ALightInTheFog -> continueNoUpgrade TheLairOfDagon
    TheLairOfDagon -> continue $ InterludeStep 4 Nothing
    InterludeStep 4 _ -> continue IntoTheMaelstrom
    IntoTheMaelstrom -> continue EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other

{- | The fourteen memories Flashback XV asks for, paired with their locale keys and in the
order the Campaign Guide prints them: the first seven make its left column, the rest its
right.
-}
flashback15Memories :: [(Memory, String)]
flashback15Memories =
  [ (AMeetingWithThomasDawson, "aMeetingWithThomasDawson")
  , (ABattleWithAHorrifyingDevil, "aBattleWithAHorrifyingDevil")
  , (ADecisionToStickTogether, "aDecisionToStickTogether")
  , (AnEncounterWithASecretCult, "anEncounterWithASecretCult")
  , (ADealWithJoeSargent, "aDealWithJoeSargent")
  , (AFollowedLead, "aFollowedLead")
  , (AnIntervention, "anIntervention")
  , (AJailbreak, "aJailbreak")
  , (DiscoveryOfAStrangeIdol, "discoveryOfAStrangeIdol")
  , (DiscoveryOfAnUnholyMantle, "discoveryOfAnUnholyMantle")
  , (DiscoveryOfAMysticalRelic, "discoveryOfAMysticalRelic")
  , (AConversationWithMrMoore, "aConversationWithMrMoore")
  , (TheLifecycleOfADeepOne, "theLifecycleOfADeepOne")
  , (AStingingBetrayal, "aStingingBetrayal")
  ]

{- | Epilogue 1 and the check it ends on. The Campaign Guide prints the fourteen memories it
asks for in two columns, so the reading ticks off the ones recovered and crosses out the ones
missing: the player can see which gap sent them to Epilogue 2. Read inside the epilogue's
scope.
-}
readEpilogue1 :: (HasI18n, ReverseQueue m) => m ()
readEpilogue1 = do
  recovered <- recoveredMemories
  let column ms = ul $ for_ ms \(m, key) -> li.validate (m `elem` recovered) ("memories." <> key)
  flavor do
    withTitle "epilogue1"
    p.basic "checkMemories"
    p.basic "ifAllFourteen"
    cols do
      column $ take 7 flashback15Memories
      column $ drop 7 flashback15Memories

-- | The fifteenth memory, recovered once all fourteen the Flashback asks for are.
recordTheHorribleTruth :: ReverseQueue m => m ()
recordTheHorribleTruth = recordSetInsert MemoriesRecovered [toJSON TheHorribleTruth]

recoveredMemories :: ReverseQueue m => m [Memory]
recoveredMemories = do
  memories <- getRecordSet MemoriesRecovered
  pure $ filter ((`elem` memories) . recorded) (map fst flashback15Memories)

instance RunMessage TheInnsmouthConspiracy where
  runMessage msg c@(TheInnsmouthConspiracy _attrs) =
    runQueueT $ campaignI18n $ lift (runInnsmouthConspiracyAchievements msg) *> case msg of
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
        when (recorded AMeetingWithThomasDawson `elem` memoriesRecovered) do
          story $ i18nWithTitle "aMeetingWithThomasDawson"
          eachInvestigator \iid -> gainXp iid CampaignSource (ikey "xp.aMeetingWithThomasDawson") 1
        when (null memoriesRecovered) $ do
          story $ i18nWithTitle "noMemoriesRecovered"
        when (recorded ABattleWithAHorrifyingDevil `elem` memoriesRecovered) do
          story $ i18nWithTitle "aBattleWithAHorrifyingDevil"
          eachInvestigator \iid -> gainXp iid CampaignSource (ikey "xp.aBattleWithAHorrifyingDevil") 1
        when (recorded ADecisionToStickTogether `elem` memoriesRecovered) do
          story $ i18nWithTitle "aDecisionToStickTogether"
          eachInvestigator \iid -> gainXp iid CampaignSource (ikey "xp.aDecisionToStickTogether") 1
        when (recorded AnEncounterWithASecretCult `elem` memoriesRecovered) do
          story $ i18nWithTitle "anEncounterWithASecretCult"
          eachInvestigator \iid -> gainXp iid CampaignSource (ikey "xp.anEncounterWithASecretCult") 1
        story $ i18nWithTitle "part2"
        nextCampaignStep
        pure c
      CampaignStep (InterludeStep 2 _) -> scope "interlude2" do
        story $ i18nWithTitle "theSyzygy1"
        whenHasRecord TheMissionFailed $ story $ i18nWithTitle "theSyzygy2"
        whenHasRecord TheMissionWasSuccessful do
          story $ i18nWithTitle "theSyzygy3"
          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.elinaHarperKnowsTooMuch
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
          removeCampaignCard Assets.elinaHarperKnowsTooMuch
          chooseOneM iid do
            questionLabeled "keepElinaHarper"
            questionLabeledCard Assets.elinaHarperKnowsTooMuch
            labeled "addElinaHarper" do
              addCampaignCardToDeck iid DoNotShuffleIn Assets.elinaHarperKnowsTooMuch
            labeled "doNotAddElinaHarper" nothing

        nextCampaignStep
        pure c
      CampaignStep EpilogueStep -> scope "epilogue" do
        readEpilogue1
        recovered <- recoveredMemories
        if all ((`elem` recovered) . fst) flashback15Memories
          then do
            recordTheHorribleTruth
            story $ i18nWithTitle "flashback15"
          else story $ i18nWithTitle "epilogue2"
        gameOver

        pure c
      _ -> lift $ defaultCampaignRunner msg c
