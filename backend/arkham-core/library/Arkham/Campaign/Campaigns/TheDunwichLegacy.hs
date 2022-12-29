module Arkham.Campaign.Campaigns.TheDunwichLegacy where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDunwichLegacy.Import
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Id
import Arkham.Message

newtype TheDunwichLegacy = TheDunwichLegacy CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

findOwner :: HasGame m => CardCode -> m (Maybe InvestigatorId)
findOwner cardCode = do
  campaignStoryCards <- getCampaignStoryCards
  pure $ findKey (any ((== cardCode) . toCardCode)) campaignStoryCards

theDunwichLegacy :: Difficulty -> TheDunwichLegacy
theDunwichLegacy difficulty = campaign
  TheDunwichLegacy
  (CampaignId "02")
  "The Dunwich Legacy"
  difficulty
  (chaosBagContents difficulty)

instance RunMessage TheDunwichLegacy where
  runMessage msg c@(TheDunwichLegacy attrs@CampaignAttrs {..}) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- allInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      c <$ pushAll
        [ storyWithChooseOne leadInvestigatorId investigatorIds prologue
          [ Label
            "Professor Warren Rice was last seen working late at night in the humanities department of Miskatonic University. Let’s search for him there. Proceed with “Scenario I–A: Extracurricular Activity” if you wish to find Professor Warren Rice first."
            [NextCampaignStep (Just $ ScenarioStep "02041")]
          , Label
            "Dr. Francis Morgan was last seen gambling at the Clover Club, an upscale speakeasy and gambling joint located downtown.  Let’s go talk to him.  Proceed with “Scenario I–B: The House Always Wins” if you wish to find Dr. Francis Morgan first."
            [NextCampaignStep (Just $ ScenarioStep "02062")]
          ]
        ]
    CampaignStep (Just (InterludeStep 1 _)) -> do
      unconsciousForSeveralHours <- getHasRecord
        InvestigatorsWereUnconsciousForSeveralHours
      investigatorIds <- allInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      if unconsciousForSeveralHours
        then c <$ pushAll
          ([ story investigatorIds armitagesFate1
           , Record DrHenryArmitageWasKidnapped
           ]
          <> [ GainXP iid 2 | iid <- investigatorIds ]
          <> [NextCampaignStep Nothing]
          )
        else c <$ pushAll
          [ story investigatorIds armitagesFate2
          , addCampaignCardToDeckChoice
            leadInvestigatorId
            investigatorIds
            Assets.drHenryArmitage
          , NextCampaignStep Nothing
          ]
    CampaignStep (Just (InterludeStep 2 _)) -> do
      sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
      investigatorIds <- allInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      drHenryArmitageUnowned <- isNothing <$> findOwner "02040"
      professorWarrenRiceUnowned <- isNothing <$> findOwner "02061"
      drFrancisMorganUnowned <- isNothing <$> findOwner "02080"
      let
        addPowderOfIbnGhazi =
          addCampaignCardToDeckChoice
              leadInvestigatorId
              investigatorIds
              Assets.powderOfIbnGhazi
            <$ guard
                 (any
                   ((`notElem` sacrificedToYogSothoth) . Recorded . toCardCode)
                   [ Assets.drHenryArmitage
                   , Assets.professorWarrenRice
                   , Assets.drFrancisMorgan
                   ]
                 )
        addDrHenryArmitage =
          addCampaignCardToDeckChoice
              leadInvestigatorId
              investigatorIds
              Assets.drHenryArmitage
            <$ guard
                 (drHenryArmitageUnowned
                 && Recorded (toCardCode Assets.drHenryArmitage)
                 `notElem` sacrificedToYogSothoth
                 )
        addProfessorWarrenRice =
          addCampaignCardToDeckChoice
              leadInvestigatorId
              investigatorIds
              Assets.professorWarrenRice
            <$ guard
                 (professorWarrenRiceUnowned
                 && Recorded (toCardCode Assets.professorWarrenRice)
                 `notElem` sacrificedToYogSothoth
                 )
        addDrFrancisMorgan =
          addCampaignCardToDeckChoice
              leadInvestigatorId
              investigatorIds
              Assets.drFrancisMorgan
            <$ guard
                 (drFrancisMorganUnowned
                 && Recorded (toCardCode Assets.drFrancisMorgan)
                 `notElem` sacrificedToYogSothoth
                 )
        addZebulonWhateley =
          addCampaignCardToDeckChoice
              leadInvestigatorId
              investigatorIds
              Assets.zebulonWhateley
            <$ guard
                 (Recorded (toCardCode Assets.zebulonWhateley)
                 `notElem` sacrificedToYogSothoth
                 )
        addEarlSawyer =
          addCampaignCardToDeckChoice
              leadInvestigatorId
              investigatorIds
              Assets.earlSawyer
            <$ guard
                 (Recorded (toCardCode Assets.earlSawyer)
                 `notElem` sacrificedToYogSothoth
                 )
      c <$ pushAll
        ([story investigatorIds interlude2]
        <> [ story investigatorIds interlude2DrHenryArmitage
           | Recorded "02040" `notElem` sacrificedToYogSothoth
           ]
        <> addDrHenryArmitage
        <> [ story investigatorIds interlude2ProfessorWarrenRice
           | Recorded "02061" `notElem` sacrificedToYogSothoth
           ]
        <> addProfessorWarrenRice
        <> [ story investigatorIds interlude2DrFrancisMorgan
           | Recorded "02080" `notElem` sacrificedToYogSothoth
           ]
        <> addDrFrancisMorgan
        <> [ story investigatorIds interlude2ZebulonWhateley
           | Recorded "02217" `notElem` sacrificedToYogSothoth
           ]
        <> addZebulonWhateley
        <> [ story investigatorIds interlude2EarlSawyer
           | Recorded "02218" `notElem` sacrificedToYogSothoth
           ]
        <> addEarlSawyer
        <> addPowderOfIbnGhazi
        <> [NextCampaignStep Nothing]
        )
    NextCampaignStep mOverrideStep -> do
      let step = mOverrideStep <|> nextStep attrs
      pushAll [CampaignStep step]
      pure
        . TheDunwichLegacy
        $ attrs
        & (stepL .~ step)
        & (completedStepsL %~ completeStep campaignStep)
    _ -> TheDunwichLegacy <$> runMessage msg attrs
