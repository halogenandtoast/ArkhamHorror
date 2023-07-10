module Arkham.Campaign.Campaigns.TheCircleUndone (
  TheCircleUndone (..),
  theCircleUndone,
) where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheCircleUndone.Import
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Token
import Arkham.Trait (Trait (SilverTwilight))

newtype Metadata = Metadata
  { prologueInvestigators :: Map InvestigatorId InvestigatorId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheCircleUndone = TheCircleUndone (CampaignAttrs `With` Metadata)
  deriving anyclass (IsCampaign)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theCircleUndone :: Difficulty -> TheCircleUndone
theCircleUndone difficulty =
  campaignWith
    (TheCircleUndone . (`with` Metadata mempty))
    (CampaignId "05")
    "The Circle Undone"
    difficulty
    (chaosBagContents difficulty)
    $ logL
      .~ mkCampaignLog
        { campaignLogRecordedSets =
            singletonMap MissingPersons $
              map (recorded . unInvestigatorId) allPrologueInvestigators
        }

allPrologueInvestigators :: [InvestigatorId]
allPrologueInvestigators = ["05046", "05047", "05048", "05049"]

instance RunMessage TheCircleUndone where
  runMessage msg c@(TheCircleUndone (attrs `With` metadata)) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- allInvestigatorIds
      pushAll $
        story investigatorIds prologue
          : [ CampaignStep (Just (InvestigatorCampaignStep iid PrologueStep))
            | iid <- investigatorIds
            ]
            <> [ story investigatorIds intro
               , CampaignStep (Just $ PrologueStepPart 2)
               , NextCampaignStep Nothing
               ]
      pure c
    CampaignStep (Just (InvestigatorCampaignStep iid PrologueStep)) -> do
      let
        availablePrologueInvestigators =
          filter
            (`notElem` toList (prologueInvestigators metadata))
            allPrologueInvestigators
      push
        $ questionLabel
          "Choose one of the following neutral investigators to control for the duration of this prologue"
          iid
        $ ChooseOne
          [ CardLabel
            (unInvestigatorId pId)
            [BecomePrologueInvestigator iid pId]
          | pId <- availablePrologueInvestigators
          ]
      pure c
    BecomePrologueInvestigator iid pId -> do
      pure . TheCircleUndone $
        attrs
          `With` metadata
            { prologueInvestigators =
                insertMap
                  iid
                  pId
                  (prologueInvestigators metadata)
            }
    CampaignStep (Just (PrologueStepPart 2)) -> do
      let
        prologueInvestigatorsNotTaken =
          map unInvestigatorId $
            allPrologueInvestigators
              \\ toList
                (prologueInvestigators metadata)
        readingFor = \case
          "05046" -> gavriellaIntro
          "05047" -> jeromeIntro
          "05048" -> valentinoIntro
          "05049" -> pennyIntro
          _ -> error "Invalid prologue investigator"
        readings = map readingFor $ toList (prologueInvestigators metadata)
      investigatorIds <- getInvestigatorIds
      pushAll $
        crossOutRecordSetEntries MissingPersons prologueInvestigatorsNotTaken
          : map (story investigatorIds) readings
      pure c
    CampaignStep (Just (InterludeStep 2 mInterludeKey)) -> do
      anySilverTwilight <- selectAny $ InvestigatorWithTrait SilverTwilight
      iids <- allInvestigatorIds
      lead <- getLead
      let
        showThePriceOfProgress4 = mInterludeKey == Just ThePriceOfProgress4
        showThePriceOfProgress5 = mInterludeKey == Just ThePriceOfProgress5
        showThePriceOfProgress6 = mInterludeKey == Just ThePriceOfProgress6
        gainXp = map (\i -> GainXP i (toSource attrs) 2) iids
        lodgeChoices =
          [ Label "\"I refuse to be part of this\"" [CampaignStep (Just (InterludeStepPart 2 mInterludeKey 7))]
          , Label "\"I agree\"" [CampaignStep (Just (InterludeStepPart 2 mInterludeKey 8))]
          , Label "\"I agree\" (You are lying)" [CampaignStep (Just (InterludeStepPart 2 mInterludeKey 9))]
          ]
      pushAll $
        [ story iids (if anySilverTwilight then thePriceOfProgress1 else thePriceOfProgress2)
        , story iids thePriceOfProgress3
        ]
          <> ( if showThePriceOfProgress4
                then
                  [ story iids thePriceOfProgress4
                  , Record JosefDisappearedIntoTheMist
                  , Record TheInvestigatorsAreEnemiesOfTheLodge
                  , NextCampaignStep Nothing
                  ]
                else []
             )
          <> ( if showThePriceOfProgress5
                then
                  [ Record TheInvestigatorsRescuedJosef
                  , storyWithChooseOne lead iids thePriceOfProgress5 lodgeChoices
                  ]
                    <> gainXp
                else []
             )
          <> ( if showThePriceOfProgress6
                then [Record JosefIsAliveAndWell, storyWithChooseOne lead iids thePriceOfProgress6 lodgeChoices]
                else []
             )
      pure c
    CampaignStep (Just (InterludeStepPart 2 _ 7)) -> do
      pushAll [Record TheInvestigatorsAreEnemiesOfTheLodge, NextCampaignStep Nothing]
      pure c
    CampaignStep (Just (InterludeStepPart 2 _ 8)) -> do
      pushAll [Record TheInvestigatorsAreMembersOfTheLodge, AddToken Cultist, NextCampaignStep Nothing]
      pure c
    CampaignStep (Just (InterludeStepPart 2 _ 9)) -> do
      pushAll
        [ Record TheInvestigatorsAreMembersOfTheLodge
        , AddToken Cultist
        , Record TheInvestigatorsAreDeceivingTheLodge
        , NextCampaignStep Nothing
        ]
      pure c
    CampaignStep (Just (InterludeStep 3 _)) -> do
      pure c
    NextCampaignStep mOverrideStep -> do
      let step = mOverrideStep <|> nextStep attrs
      push $ CampaignStep step
      pure
        . TheCircleUndone
        . (`with` metadata)
        $ attrs
          & (stepL .~ step)
          & (completedStepsL %~ completeStep (campaignStep attrs))
    PreScenarioSetup -> do
      case mapToList (prologueInvestigators metadata) of
        [] -> pure ()
        xs -> pushAll $ map (uncurry BecomePrologueInvestigator) xs
      pure c
    EndOfScenario _ -> do
      pure . TheCircleUndone $
        attrs
          `With` metadata
            { prologueInvestigators = mempty
            }
    ResetGame -> do
      case mapToList (prologueInvestigators metadata) of
        [] -> TheCircleUndone . (`with` metadata) <$> runMessage msg attrs
        xs -> do
          for_ xs $ \(iid, _) -> push $ LoadDeck iid $ Deck []
          pure c
    _ -> TheCircleUndone . (`with` metadata) <$> runMessage msg attrs
