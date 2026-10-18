module Arkham.Campaign.Campaigns.TheDunwichLegacy (theDunwichLegacy, TheDunwichLegacy (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Campaigns.TheDunwichLegacy.Achievements (runDunwichAchievements)
import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Option
import Arkham.Campaign.Types
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDunwichLegacy.CampaignSteps
import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Campaigns.TheDunwichLegacy.Import
import Arkham.Card
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Narrative
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Narrative.TheDunwichLegacy
import Arkham.Resolution
import Arkham.Treachery.CardDefs.TheDunwichLegacy.TheEssexCountyExpress qualified as Treacheries
import GHC.Records

newtype TheDunwichLegacy = TheDunwichLegacy CampaignAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

instance HasField "attrs" TheDunwichLegacy CampaignAttrs where
  getField (TheDunwichLegacy attrs) = attrs

instance IsCampaign TheDunwichLegacy where
  campaignTokens = chaosBagContents
  invalidCards =
    recordedCardCodes
      . findWithDefault [] (toCampaignLogKey SacrificedToYogSothoth)
      . attr (campaignLogRecordedSets . campaignLog)

  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> error $ "Unhandled campaign step: " <> show a
    ExtracurricularActivity ->
      if TheHouseAlwaysWins `elem` a.attrs.completedSteps
        then continue $ InterludeStep 1 Nothing
        else continue TheHouseAlwaysWins
    TheHouseAlwaysWins ->
      if ExtracurricularActivity `elem` a.attrs.completedSteps
        then continue $ InterludeStep 1 Nothing
        else continue ExtracurricularActivity
    InterludeStep 1 _ -> continue TheMiskatonicMuseum
    -- A narrative may add an interlude here, so a group that wants a side
    -- scenario has a reason to take one before boarding the train.
    TheMiskatonicMuseum
      | addsFalseLeads a.attrs.log.options -> continue FalseLeads
    TheMiskatonicMuseum -> continue TheEssexCountyExpress
    FalseLeads -> continue TheEssexCountyExpress
    TheEssexCountyExpress -> continue BloodOnTheAltar
    BloodOnTheAltar ->
      case lookup "02195" a.attrs.resolutions of
        Just NoResolution -> continue UndimensionedAndUnseen
        _ -> continue $ InterludeStep 2 Nothing
    InterludeStep 2 _ -> continue UndimensionedAndUnseen
    UndimensionedAndUnseen -> continue WhereDoomAwaits
    WhereDoomAwaits -> continue LostInTimeAndSpace
    LostInTimeAndSpace -> continue EpilogueStep
    other -> defaultNextStep other

theDunwichLegacy :: Difficulty -> TheDunwichLegacy
theDunwichLegacy = campaign TheDunwichLegacy (CampaignId "02") "The Dunwich Legacy"

instance RunMessage TheDunwichLegacy where
  runMessage msg c =
    runQueueT $ campaignI18n $ lift (runDunwichAchievements msg) *> case msg of
      CampaignStep PrologueStep -> scope "prologue" do
        narrative <- getDunwichNarrative
        body <- narrativeBuilder narrative.prologue (setTitle "title" >> p "body")
        storyWithChooseOneM body do
          labeled "extracurricularActivity" $ setNextCampaignStep ExtracurricularActivity
          labeled "theHouseAlwaysWins" $ setNextCampaignStep TheHouseAlwaysWins
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
            narrative <- getDunwichNarrative
            storyBuild
              =<< narrativeBuilder narrative.armitagesFate1 (setTitle "title" >> p "armitagesFate1")
            record DrHenryArmitageWasKidnapped
            interludeXpAll $ toBonus "bonus" 2
          else do
            interlude "armitagesFate2"
            record TheInvestigatorsRescuedDrHenryArmitage

            investigators <- allInvestigators
            addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.drHenryArmitage

        nextCampaignStep
        pure c
      CampaignStep FalseLeads -> scope "interlude1a" do
        -- The step exists only under a narrative that adds it, so the Campaign
        -- Guide has no reading of it to fall back to.
        narrative <- getDunwichNarrative
        storyBuild =<< narrativeBuilder narrative.interlude1a (pure ())
        nextCampaignStep
        pure c
      CampaignStep (InterludeStep 2 _) -> scope "interlude2" do
        narrative <- getDunwichNarrative
        sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
        let sacrificed = (`elem` sacrificedToYogSothoth) . recorded . toCardCode
        investigators <- allInvestigators
        let interlude k = storyBuild $ setTitle "title" >> p k

        interlude "body"

        unless (sacrificed Assets.drHenryArmitage) do
          armitageKey <- narrativeKey "drHenryArmitage" narrative.interlude2Armitage
          interlude armitageKey
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
          onNarrative narrative.interlude2FrancisMorgan
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
        narrative <- getDunwichNarrative
        storyBuild
          =<< narrativeBuilder
            narrative.epilogue
            ( do
                setTitle "title"
                p "intro"
                p $ if warned then "epilogue2" else "epilogue1"
            )
        gameOverWon
        pure c
      HandleOption PlayersDoNotControlStoryAssetClues -> do
        pure $ TheDunwichLegacy $ c.attrs & logL . optionsL %~ insertSet PlayersDoNotControlStoryAssetClues
      HandleOption opt@(NarrativeVariant _) -> do
        pure $ TheDunwichLegacy $ c.attrs & logL . optionsL %~ insertSet opt
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
            chooseSome1M lead "acrossSpaceAndTime" do
              for_ (zip investigators acrossSpaceAndTimes) \(iid, acrossSpaceAndTime) ->
                portraitLabeled iid $ addCampaignCardToDeck iid ShuffleIn acrossSpaceAndTime
          Cheated -> addChaosToken #elderthing
          _ -> error $ "Unhandled option: " <> show option
        pure c
      _ -> lift $ defaultCampaignRunner msg c
