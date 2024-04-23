module Arkham.Campaign.Campaigns.TheDunwichLegacy where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.Campaign.Runner
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheDunwichLegacy.Import
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Id
import Arkham.Resolution
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheDunwichLegacy = TheDunwichLegacy CampaignAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

instance IsCampaign TheDunwichLegacy where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> error $ "Unhandled campaign step: " <> show a
    ExtracurricularActivity ->
      if TheHouseAlwaysWins `elem` campaignCompletedSteps (toAttrs a)
        then Just $ InterludeStep 1 Nothing
        else Just (UpgradeDeckStep TheHouseAlwaysWins)
    TheHouseAlwaysWins ->
      if ExtracurricularActivity `elem` campaignCompletedSteps (toAttrs a)
        then Just $ InterludeStep 1 Nothing
        else Just (UpgradeDeckStep ExtracurricularActivity)
    InterludeStep 1 _ -> Just (UpgradeDeckStep TheMiskatonicMuseum)
    TheMiskatonicMuseum -> Just (UpgradeDeckStep TheEssexCountyExpress)
    TheEssexCountyExpress -> Just (UpgradeDeckStep BloodOnTheAltar)
    BloodOnTheAltar ->
      case lookup "02195" (campaignResolutions $ toAttrs a) of
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
  runMessage msg c = case msg of
    CampaignStep PrologueStep -> do
      players <- allPlayers
      lead <- getLeadPlayer
      pushAll
        [ storyWithChooseOne
            lead
            players
            prologue
            [ Label
                "Professor Warren Rice was last seen working late at night in the humanities department of Miskatonic University. Let’s search for him there. Proceed with “Scenario I–A: Extracurricular Activity” if you wish to find Professor Warren Rice first."
                [NextCampaignStep (Just ExtracurricularActivity)]
            , Label
                "Dr. Francis Morgan was last seen gambling at the Clover Club, an upscale speakeasy and gambling joint located downtown.  Let’s go talk to him.  Proceed with “Scenario I–B: The House Always Wins” if you wish to find Dr. Francis Morgan first."
                [NextCampaignStep (Just TheHouseAlwaysWins)]
            ]
        ]
      pure c
    CampaignStep (InterludeStep 1 _) -> do
      unconsciousForSeveralHours <-
        getHasRecord
          InvestigatorsWereUnconsciousForSeveralHours
      investigatorIds <- allInvestigatorIds
      players <- allPlayers
      lead <- getLeadPlayer
      if unconsciousForSeveralHours
        then
          pushAll
            $ [ story players armitagesFate1
              , Record DrHenryArmitageWasKidnapped
              ]
            <> [GainXP iid CampaignSource 2 | iid <- investigatorIds]
            <> [NextCampaignStep Nothing]
        else
          pushAll
            [ story players armitagesFate2
            , Record TheInvestigatorsRescuedDrHenryArmitage
            , addCampaignCardToDeckChoice
                lead
                investigatorIds
                Assets.drHenryArmitage
            , NextCampaignStep Nothing
            ]
      pure c
    CampaignStep (InterludeStep 2 _) -> do
      sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
      investigatorIds <- allInvestigatorIds
      players <- allPlayers
      lead <- getLeadPlayer
      drHenryArmitageUnowned <- isNothing <$> getOwner Assets.drHenryArmitage
      professorWarrenRiceUnowned <- isNothing <$> getOwner Assets.professorWarrenRice
      drFrancisMorganUnowned <- isNothing <$> getOwner Assets.drFrancisMorgan
      let
        addPowderOfIbnGhazi =
          addCampaignCardToDeckChoice
            lead
            investigatorIds
            Assets.powderOfIbnGhazi
            <$ guard
              ( any
                  ((`notElem` sacrificedToYogSothoth) . recorded . toCardCode)
                  [ Assets.drHenryArmitage
                  , Assets.professorWarrenRice
                  , Assets.drFrancisMorgan
                  ]
              )
        addDrHenryArmitage =
          addCampaignCardToDeckChoice
            lead
            investigatorIds
            Assets.drHenryArmitage
            <$ guard
              ( drHenryArmitageUnowned
                  && recorded (toCardCode Assets.drHenryArmitage)
                  `notElem` sacrificedToYogSothoth
              )
        addProfessorWarrenRice =
          addCampaignCardToDeckChoice
            lead
            investigatorIds
            Assets.professorWarrenRice
            <$ guard
              ( professorWarrenRiceUnowned
                  && recorded (toCardCode Assets.professorWarrenRice)
                  `notElem` sacrificedToYogSothoth
              )
        addDrFrancisMorgan =
          addCampaignCardToDeckChoice
            lead
            investigatorIds
            Assets.drFrancisMorgan
            <$ guard
              ( drFrancisMorganUnowned
                  && recorded (toCardCode Assets.drFrancisMorgan)
                  `notElem` sacrificedToYogSothoth
              )
        addZebulonWhateley =
          addCampaignCardToDeckChoice
            lead
            investigatorIds
            Assets.zebulonWhateley
            <$ guard
              ( recorded (toCardCode Assets.zebulonWhateley)
                  `notElem` sacrificedToYogSothoth
              )
        addEarlSawyer =
          addCampaignCardToDeckChoice
            lead
            investigatorIds
            Assets.earlSawyer
            <$ guard
              ( recorded (toCardCode Assets.earlSawyer)
                  `notElem` sacrificedToYogSothoth
              )
      pushAll
        $ [story players interlude2]
        <> ( guard (recorded @CardCode "02040" `notElem` sacrificedToYogSothoth)
              *> [story players interlude2DrHenryArmitage, Record DrHenryArmitageSurvivedTheDunwichLegacy]
           )
        <> addDrHenryArmitage
        <> ( guard (recorded @CardCode "02061" `notElem` sacrificedToYogSothoth)
              *> [story players interlude2ProfessorWarrenRice, Record ProfessorWarrenRiceSurvivedTheDunwichLegacy]
           )
        <> addProfessorWarrenRice
        <> ( guard (recorded @CardCode "02080" `notElem` sacrificedToYogSothoth)
              *> [story players interlude2DrFrancisMorgan, Record DrFrancisMorganSurvivedTheDunwichLegacy]
           )
        <> addDrFrancisMorgan
        <> ( guard (recorded @CardCode "02217" `notElem` sacrificedToYogSothoth)
              *> [story players interlude2ZebulonWhateley, Record ZebulonWhateleySurvivedTheDunwichLegacy]
           )
        <> addZebulonWhateley
        <> ( guard (recorded @CardCode "02218" `notElem` sacrificedToYogSothoth)
              *> [story players interlude2EarlSawyer, Record EarlSawyerSurvivedTheDunwichLegacy]
           )
        <> addEarlSawyer
        <> addPowderOfIbnGhazi
        <> [NextCampaignStep Nothing]
      pure c
    CampaignStep EpilogueStep -> do
      warned <- getHasRecord YouWarnedTheTownsfolk
      players <- allPlayers
      pushAll [story players $ if warned then epilogue2 else epilogue1, GameOver]
      pure c
    HandleOption option -> do
      lead <- getActivePlayer
      investigators <- allInvestigators
      sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
      let sacrificed = (`elem` sacrificedToYogSothoth) . recorded . toCardCode
      case option of
        TakeArmitage -> do
          unless (sacrificed Assets.drHenryArmitage)
            $ push
            $ forceAddCampaignCardToDeckChoice lead investigators Assets.drHenryArmitage
        TakeWarrenRice -> do
          unless (sacrificed Assets.professorWarrenRice)
            $ push
            $ forceAddCampaignCardToDeckChoice lead investigators Assets.professorWarrenRice
        TakeFrancisMorgan -> do
          unless (sacrificed Assets.drFrancisMorgan)
            $ push
            $ forceAddCampaignCardToDeckChoice lead investigators Assets.drFrancisMorgan
        TakeZebulonWhately -> push $ forceAddCampaignCardToDeckChoice lead investigators Assets.zebulonWhateley
        TakeEarlSawyer -> push $ forceAddCampaignCardToDeckChoice lead investigators Assets.earlSawyer
        TakePowderOfIbnGhazi -> do
          when (campaignStep (toAttrs c) == UndimensionedAndUnseen)
            $ push
            $ forceAddCampaignCardToDeckChoice lead investigators Assets.powderOfIbnGhazi
        TakeTheNecronomicon -> do
          stolen <- getHasRecord TheNecronomiconWasStolen
          unless stolen
            $ push
            $ forceAddCampaignCardToDeckChoice lead investigators Assets.theNecronomiconOlausWormiusTranslation
        AddAcrossTimeAndSpace -> do
          push
            $ Ask lead
            $ ChooseSome1
              "Do not add Across Time and Space to any other decks"
              [ PortraitLabel iid [AddCampaignCardToDeck iid Treacheries.acrossSpaceAndTime]
              | iid <- investigators
              ]
        Cheated -> push $ AddChaosToken #elderthing
        _ -> error $ "Unhandled option: " <> show option
      pure c
    _ -> defaultCampaignRunner msg c
