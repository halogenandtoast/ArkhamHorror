module Arkham.Campaign.Campaigns.TheDunwichLegacy where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Option
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDunwichLegacy.CampaignSteps
import Arkham.Campaigns.TheDunwichLegacy.Import
import Arkham.Card
import Arkham.Game.Helpers
import Arkham.Helpers.Campaign (getOwner)
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
  runMessage msg c = runQueueT $ case msg of
    CampaignStep PrologueStep -> do
      storyWithChooseOneM prologue do
        labeled
          "Professor Warren Rice was last seen working late at night in the humanities department of Miskatonic University. Let’s search for him there. Proceed with “Scenario I–A: Extracurricular Activity” if you wish to find Professor Warren Rice first."
          $ setNextCampaignStep ExtracurricularActivity
        labeled
          "Dr. Francis Morgan was last seen gambling at the Clover Club, an upscale speakeasy and gambling joint located downtown.  Let’s go talk to him.  Proceed with “Scenario I–B: The House Always Wins” if you wish to find Dr. Francis Morgan first."
          $ setNextCampaignStep TheHouseAlwaysWins
      pure c
    CampaignStep (InterludeStep 1 _) -> do
      unconsciousForSeveralHours <- getHasRecord InvestigatorsWereUnconsciousForSeveralHours
      if unconsciousForSeveralHours
        then do
          story armitagesFate1
          record DrHenryArmitageWasKidnapped
          interludeXpAll
            (WithBonus "Reading Wilbur’s journal gives them insight into the hidden world of the mythos." 2)
        else do
          story armitagesFate2
          record TheInvestigatorsRescuedDrHenryArmitage

          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators Assets.drHenryArmitage

      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 2 _) -> do
      sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
      let sacrificed = (`elem` sacrificedToYogSothoth) . recorded . toCardCode
      investigators <- allInvestigators

      story interlude2

      unless (sacrificed Assets.drHenryArmitage) do
        story interlude2DrHenryArmitage
        record DrHenryArmitageSurvivedTheDunwichLegacy

        whenM (isNothing <$> getOwner Assets.drHenryArmitage) do
          addCampaignCardToDeckChoice investigators Assets.drHenryArmitage

      unless (sacrificed Assets.professorWarrenRice) do
        story interlude2ProfessorWarrenRice
        record ProfessorWarrenRiceSurvivedTheDunwichLegacy

        whenM (isNothing <$> getOwner Assets.professorWarrenRice) do
          addCampaignCardToDeckChoice investigators Assets.professorWarrenRice

      unless (sacrificed Assets.drFrancisMorgan) do
        story interlude2DrFrancisMorgan
        record DrFrancisMorganSurvivedTheDunwichLegacy

        whenM (isNothing <$> getOwner Assets.drFrancisMorgan) do
          addCampaignCardToDeckChoice investigators Assets.drFrancisMorgan

      unless (sacrificed Assets.zebulonWhateley) do
        story interlude2ZebulonWhateley
        record ZebulonWhateleySurvivedTheDunwichLegacy
        addCampaignCardToDeckChoice investigators Assets.zebulonWhateley

      unless (sacrificed Assets.earlSawyer) do
        story interlude2EarlSawyer
        record EarlSawyerSurvivedTheDunwichLegacy
        addCampaignCardToDeckChoice investigators Assets.earlSawyer

      unless
        (all sacrificed [Assets.drHenryArmitage, Assets.professorWarrenRice, Assets.drFrancisMorgan])
        do
          addCampaignCardToDeckChoice investigators Assets.powderOfIbnGhazi

      nextCampaignStep
      pure c
    CampaignStep EpilogueStep -> do
      warned <- getHasRecord YouWarnedTheTownsfolk
      story $ if warned then epilogue2 else epilogue1
      gameOver
      pure c
    HandleOption option -> do
      investigators <- allInvestigators
      sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
      let sacrificed = (`elem` sacrificedToYogSothoth) . recorded . toCardCode
      case option of
        TakeArmitage -> do
          unless (sacrificed Assets.drHenryArmitage) do
            forceAddCampaignCardToDeckChoice investigators Assets.drHenryArmitage
        TakeWarrenRice -> do
          unless (sacrificed Assets.professorWarrenRice) do
            forceAddCampaignCardToDeckChoice investigators Assets.professorWarrenRice
        TakeFrancisMorgan -> do
          unless (sacrificed Assets.drFrancisMorgan) do
            forceAddCampaignCardToDeckChoice investigators Assets.drFrancisMorgan
        TakeZebulonWhately -> forceAddCampaignCardToDeckChoice investigators Assets.zebulonWhateley
        TakeEarlSawyer -> forceAddCampaignCardToDeckChoice investigators Assets.earlSawyer
        TakePowderOfIbnGhazi -> when (c.attrs.step == UndimensionedAndUnseen) do
          forceAddCampaignCardToDeckChoice investigators Assets.powderOfIbnGhazi
        TakeTheNecronomicon -> unlessHasRecord TheNecronomiconWasStolen do
          forceAddCampaignCardToDeckChoice investigators Assets.theNecronomiconOlausWormiusTranslation
        AddAcrossTimeAndSpace -> do
          acrossSpaceAndTimes <- replicateM 4 (genCard Treacheries.acrossSpaceAndTime)
          lead <- getActiveInvestigatorId
          chooseSome1M lead "Do not add Across Time and Space to any other decks" do
            for_ (zip investigators acrossSpaceAndTimes) \(iid, acrossSpaceAndTime) ->
              portraitLabeled iid $ addCampaignCardToDeck iid acrossSpaceAndTime
        Cheated -> addChaosToken #elderthing
        _ -> error $ "Unhandled option: " <> show option
      pure c
    _ -> lift $ defaultCampaignRunner msg c
