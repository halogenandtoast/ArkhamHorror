module Arkham.Campaign.Campaigns.TheDunwichLegacy (theDunwichLegacy, TheDunwichLegacy (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Option
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDunwichLegacy.CampaignSteps
import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Campaigns.TheDunwichLegacy.Import
import Arkham.Card
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Treachery.Cards qualified as Treacheries
import GHC.Records

newtype TheDunwichLegacy = TheDunwichLegacy CampaignAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

instance HasField "attrs" TheDunwichLegacy CampaignAttrs where
  getField (TheDunwichLegacy attrs) = attrs

instance IsCampaign TheDunwichLegacy where
  nextStep a = case a.attrs.step of
    PrologueStep -> error $ "Unhandled campaign step: " <> show a
    ExtracurricularActivity ->
      if TheHouseAlwaysWins `elem` a.attrs.completedSteps
        then Just $ InterludeStep 1 Nothing
        else Just (UpgradeDeckStep TheHouseAlwaysWins)
    TheHouseAlwaysWins ->
      if ExtracurricularActivity `elem` a.attrs.completedSteps
        then Just $ InterludeStep 1 Nothing
        else Just (UpgradeDeckStep ExtracurricularActivity)
    InterludeStep 1 _ -> Just (UpgradeDeckStep TheMiskatonicMuseum)
    TheMiskatonicMuseum -> Just (UpgradeDeckStep TheEssexCountyExpress)
    TheEssexCountyExpress -> Just (UpgradeDeckStep BloodOnTheAltar)
    BloodOnTheAltar ->
      case lookup "02195" a.attrs.resolutions of
        Just NoResolution -> Just (UpgradeDeckStep UndimensionedAndUnseen)
        _ -> Just $ InterludeStep 2 Nothing
    InterludeStep 2 _ -> Just (UpgradeDeckStep UndimensionedAndUnseen)
    UndimensionedAndUnseen -> Just (UpgradeDeckStep WhereDoomAwaits)
    WhereDoomAwaits -> Just (UpgradeDeckStep LostInTimeAndSpace)
    LostInTimeAndSpace -> Just EpilogueStep
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

theDunwichLegacy :: Difficulty -> TheDunwichLegacy
theDunwichLegacy difficulty =
  campaign
    TheDunwichLegacy
    (CampaignId "02")
    "The Dunwich Legacy"
    difficulty
    (chaosBagContents difficulty)

instance RunMessage TheDunwichLegacy where
  runMessage msg c = runQueueT $ campaignI18n $ case msg of
    CampaignStep PrologueStep -> scope "prologue" do
      storyWithChooseOneM' (setTitle "title" >> p "body") do
        labeled' "extracurricularActivity" $ setNextCampaignStep ExtracurricularActivity
        labeled' "theHouseAlwaysWins" $ setNextCampaignStep TheHouseAlwaysWins
      pure c
    CampaignStep (InterludeStep 1 _) -> scope "interlude1" do
      unconsciousForSeveralHours <- getHasRecord InvestigatorsWereUnconsciousForSeveralHours
      let interlude k = storyBuild $ setTitle "title" >> p k
      storyBuild do
        setTitle "title"
        p.validate unconsciousForSeveralHours "proceedToArmitagesFate1"
        p.validate (not unconsciousForSeveralHours) "proceedToArmitagesFate2"
      if unconsciousForSeveralHours
        then do
          interlude "armitagesFate1"
          record DrHenryArmitageWasKidnapped
          interludeXpAll $ toBonus "bonus" 2
        else do
          interlude "armitagesFate2"
          record TheInvestigatorsRescuedDrHenryArmitage

          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.drHenryArmitage

      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 2 _) -> scope "interlude2" do
      sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
      let sacrificed = (`elem` sacrificedToYogSothoth) . recorded . toCardCode
      investigators <- allInvestigators
      let interlude k = storyBuild $ setTitle "title" >> p k

      interlude "body"

      unless (sacrificed Assets.drHenryArmitage) do
        interlude "drHenryArmitage"
        record DrHenryArmitageSurvivedTheDunwichLegacy

        whenM (isNothing <$> getOwner Assets.drHenryArmitage) do
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.drHenryArmitage

      unless (sacrificed Assets.professorWarrenRice) do
        interlude "professorWarrenRice"
        record ProfessorWarrenRiceSurvivedTheDunwichLegacy

        whenM (isNothing <$> getOwner Assets.professorWarrenRice) do
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.professorWarrenRice

      unless (sacrificed Assets.drFrancisMorgan) do
        interlude "drFrancisMorgan"
        record DrFrancisMorganSurvivedTheDunwichLegacy

        whenM (isNothing <$> getOwner Assets.drFrancisMorgan) do
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.drFrancisMorgan

      unless (sacrificed Assets.zebulonWhateley) do
        interlude "zebulonWhateley"
        record ZebulonWhateleySurvivedTheDunwichLegacy
        addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.zebulonWhateley

      unless (sacrificed Assets.earlSawyer) do
        interlude "earlSawyer"
        record EarlSawyerSurvivedTheDunwichLegacy
        addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.earlSawyer

      unless
        (all sacrificed [Assets.drHenryArmitage, Assets.professorWarrenRice, Assets.drFrancisMorgan])
        do
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.powderOfIbnGhazi

      nextCampaignStep
      pure c
    CampaignStep EpilogueStep -> scope "epilogue" do
      warned <- getHasRecord YouWarnedTheTownsfolk
      storyBuild do
        setTitle "title"
        p "intro"
        p $ if warned then "epilogue2" else "epilogue1"
      gameOver
      pure c
    HandleOption option -> do
      investigators <- allInvestigators
      sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
      let sacrificed = (`elem` sacrificedToYogSothoth) . recorded . toCardCode
      case option of
        TakeArmitage -> do
          unless (sacrificed Assets.drHenryArmitage) do
            forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.drHenryArmitage
        TakeWarrenRice -> do
          unless (sacrificed Assets.professorWarrenRice) do
            forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.professorWarrenRice
        TakeFrancisMorgan -> do
          unless (sacrificed Assets.drFrancisMorgan) do
            forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.drFrancisMorgan
        TakeZebulonWhately -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.zebulonWhateley
        TakeEarlSawyer -> forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.earlSawyer
        TakePowderOfIbnGhazi -> when (c.attrs.step == UndimensionedAndUnseen) do
          forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.powderOfIbnGhazi
        TakeTheNecronomicon -> unlessHasRecord TheNecronomiconWasStolen do
          forceAddCampaignCardToDeckChoice
            investigators
            ShuffleIn
            Assets.theNecronomiconOlausWormiusTranslation
        AddAcrossSpaceAndTime -> scope "options" do
          acrossSpaceAndTimes <- replicateM 4 (genCard Treacheries.acrossSpaceAndTime)
          lead <- getActiveInvestigatorId
          chooseSome1M' lead "acrossSpaceAndTime" do
            for_ (zip investigators acrossSpaceAndTimes) \(iid, acrossSpaceAndTime) ->
              portraitLabeled iid $ addCampaignCardToDeck iid ShuffleIn acrossSpaceAndTime
        Cheated -> addChaosToken #elderthing
        _ -> error $ "Unhandled option: " <> show option
      pure c
    _ -> lift $ defaultCampaignRunner msg c
