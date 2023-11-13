module Arkham.Campaign.Campaigns.TheDreamEaters (
  TheDreamEaters (..),
  theDreamEaters,
) where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.CampaignStep
import Arkham.Campaigns.TheDreamEaters.CampaignSteps
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Investigator (Investigator, lookupInvestigator)
import Arkham.Investigator.Types (InvestigatorAttrs)
import Arkham.Projection

data CampaignPart = TheDreamQuest | TheWebOfDreams
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CampaignMode = PartialMode CampaignPart | FullMode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Metadata = Metadata
  { campaignMode :: CampaignMode
  , currentCampaignMode :: Maybe CampaignPart
  , otherCampaignAttrs :: Maybe CampaignAttrs
  , currentCampaignPlayers :: Map PlayerId InvestigatorAttrs
  , otherCampaignPlayers :: Map PlayerId InvestigatorAttrs
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheDreamEaters = TheDreamEaters (CampaignAttrs `With` Metadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theDreamEaters :: Difficulty -> TheDreamEaters
theDreamEaters difficulty =
  campaign
    (TheDreamEaters . (`with` Metadata FullMode Nothing Nothing mempty mempty))
    (CampaignId "06")
    "The Dream-Eaters"
    difficulty
    [] -- We will set this later

instance IsCampaign TheDreamEaters where
  nextStep a@(TheDreamEaters (_ `With` meta)) = case campaignStep (toAttrs a) of
    PrologueStep -> error $ "Unhandled campaign step: " <> show a
    BeyondTheGatesOfSleep ->
      case campaignMode meta of
        FullMode ->
          if WakingNightmare `elem` campaignCompletedSteps (toAttrs a)
            then Just $ InterludeStep 1 Nothing
            else Just (UpgradeDeckStep WakingNightmare)
        PartialMode _ -> Just $ InterludeStep 1 Nothing
    WakingNightmare ->
      case campaignMode meta of
        FullMode ->
          if BeyondTheGatesOfSleep `elem` campaignCompletedSteps (toAttrs a)
            then Just $ InterludeStep 1 Nothing
            else Just (UpgradeDeckStep BeyondTheGatesOfSleep)
        PartialMode _ -> Just $ InterludeStep 1 Nothing
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

theDreamQuestSteps :: [CampaignStep]
theDreamQuestSteps = [BeyondTheGatesOfSleep, TheSearchForKadath, DarkSideOfTheMoon, WhereTheGodsDwell]

theWebOfDreamsSteps :: [CampaignStep]
theWebOfDreamsSteps = [WakingNightmare, AThousandShapesOfHorror, PointOfNoReturn, WeaverOfTheCosmos]

initChaosBag :: CampaignPart -> Difficulty -> [ChaosTokenFace]
initChaosBag TheDreamQuest = \case
  Easy ->
    [ PlusOne
    , PlusOne
    , Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , Cultist
    , Tablet
    , Tablet
    , AutoFail
    , ElderSign
    ]
  Standard ->
    [ PlusOne
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusFour
    , Cultist
    , Tablet
    , Tablet
    , AutoFail
    , ElderSign
    ]
  Hard ->
    [ Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFive
    , Cultist
    , Tablet
    , Tablet
    , AutoFail
    , ElderSign
    ]
  Expert ->
    [ Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , MinusEight
    , Cultist
    , Tablet
    , Tablet
    , AutoFail
    , ElderSign
    ]
initChaosBag TheWebOfDreams = \case
  Easy ->
    [ PlusOne
    , PlusOne
    , Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , Skull
    , Skull
    , Cultist
    , ElderThing
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Standard ->
    [ PlusOne
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusFour
    , Skull
    , Skull
    , Cultist
    , ElderThing
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Hard ->
    [ Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFive
    , Skull
    , Skull
    , Cultist
    , ElderThing
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Expert ->
    [ Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , MinusEight
    , Skull
    , Skull
    , Cultist
    , ElderThing
    , ElderThing
    , AutoFail
    , ElderSign
    ]

instance RunMessage TheDreamEaters where
  runMessage msg c@(TheDreamEaters (attrs `With` meta)) = case msg of
    StartCampaign -> do
      -- [ALERT] StartCampaign, overriden to not choose decks yet
      lead <- getActivePlayer
      pushAll
        $ [Ask lead PickCampaignSettings | campaignStep attrs /= PrologueStep]
        <> [CampaignStep $ campaignStep attrs]
      pure c
    CampaignStep PrologueStep -> do
      lead <- getActivePlayer
      push
        $ questionLabel "Which mode would you like to play" lead
        $ ChooseOne
          [ Label "Full Campaign" [CampaignStep (PrologueStepPart 1)]
          , Label "The Dream-Quest" [CampaignStep (PrologueStepPart 2)]
          , Label "The Web of Dreams" [CampaignStep (PrologueStepPart 3)]
          ]
      pure c
    CampaignStep (PrologueStepPart 1) -> do
      lead <- getActivePlayer
      push
        $ questionLabel "Which scenario would you like to start with" lead
        $ ChooseOne
          [ Label "Beyond the Gates of Sleep" [CampaignStep (PrologueStepPart 11)]
          , Label "Waking Nightmare" [CampaignStep (PrologueStepPart 12)]
          ]
      pure c
    CampaignStep (PrologueStepPart 2) -> do
      players <- allPlayers
      pushAll
        $ map chooseDeck players
        <> [NextCampaignStep (Just BeyondTheGatesOfSleep)]
      pure
        $ TheDreamEaters
        $ attrs {campaignChaosBag = initChaosBag TheDreamQuest (campaignDifficulty attrs)}
        `with` meta {campaignMode = PartialMode TheDreamQuest}
    CampaignStep (PrologueStepPart 3) -> do
      players <- allPlayers
      pushAll
        $ map chooseDeck players
        <> [NextCampaignStep (Just WakingNightmare)]
      pure
        $ TheDreamEaters
        $ attrs {campaignChaosBag = initChaosBag TheWebOfDreams (campaignDifficulty attrs)}
        `with` meta {campaignMode = PartialMode TheWebOfDreams}
    CampaignStep (PrologueStepPart 11) -> do
      players <- allPlayers
      pushAll
        $ map (\pid -> questionLabel "Choose Deck For Part A" pid ChooseDeck) players
        <> [NextCampaignStep (Just BeyondTheGatesOfSleep)]
      let difficulty = campaignDifficulty attrs
      pure
        $ TheDreamEaters
        $ attrs {campaignChaosBag = initChaosBag TheDreamQuest difficulty}
        `with` meta
          { currentCampaignMode = Just TheDreamQuest
          , otherCampaignAttrs = Just (attrs {campaignChaosBag = initChaosBag TheWebOfDreams difficulty})
          }
    CampaignStep (PrologueStepPart 12) -> do
      players <- allPlayers
      pushAll
        $ map (\pid -> questionLabel "Choose Deck For Part B" pid ChooseDeck) players
        <> [NextCampaignStep (Just WakingNightmare)]
      let difficulty = campaignDifficulty attrs
      pure
        $ TheDreamEaters
        $ attrs {campaignChaosBag = initChaosBag TheWebOfDreams difficulty}
        `with` meta
          { currentCampaignMode = Just TheWebOfDreams
          , otherCampaignAttrs = Just (attrs {campaignChaosBag = initChaosBag TheDreamQuest difficulty})
          }
    CampaignStep s@(ScenarioStep _) -> do
      void $ defaultCampaignRunner msg c
      case s of
        _ | s `elem` theDreamQuestSteps -> do
          if currentCampaignMode meta == Just TheWebOfDreams
            then do
              investigators <- allInvestigators
              currentPlayers <- for investigators \i -> do
                player <- getPlayer i
                iattrs <- getAttrs @Investigator i
                pure (player, iattrs)

              if (s == BeyondTheGatesOfSleep && WakingNightmare `elem` campaignCompletedSteps attrs)
                then do
                  players <- allPlayers
                  pushAll $ map (\pid -> questionLabel "Choose Deck For Part A" pid ChooseDeck) players
                else do
                  for_ (mapToList $ otherCampaignPlayers meta) \(pid, iattrs) -> do
                    let i = overAttrs (const iattrs) $ lookupInvestigator (toId iattrs) pid
                    push $ SetInvestigator pid i
              let newAttrs = fromJustNote "not full campaign" (otherCampaignAttrs meta)
              pure
                $ TheDreamEaters
                  ( newAttrs
                      { campaignCompletedSteps = campaignCompletedSteps attrs
                      , campaignStep = s
                      , campaignLog = campaignLog attrs
                      , campaignResolutions = campaignResolutions attrs
                      , campaignModifiers = campaignModifiers attrs
                      }
                      `with` meta
                        { currentCampaignMode = Just TheDreamQuest
                        , otherCampaignAttrs = Just attrs
                        , currentCampaignPlayers = otherCampaignPlayers meta
                        , otherCampaignPlayers = mapFromList currentPlayers
                        }
                  )
            else pure $ TheDreamEaters (attrs `with` meta)
        _ | s `elem` theWebOfDreamsSteps -> do
          if currentCampaignMode meta == Just TheDreamQuest
            then do
              investigators <- allInvestigators
              currentPlayers <- for investigators \i -> do
                player <- getPlayer i
                iattrs <- getAttrs @Investigator i
                pure (player, iattrs)
              if (s == WakingNightmare && BeyondTheGatesOfSleep `elem` campaignCompletedSteps attrs)
                then do
                  players <- allPlayers
                  pushAll $ map (\pid -> questionLabel "Choose Deck For Part B" pid ChooseDeck) players
                else do
                  for_ (mapToList $ otherCampaignPlayers meta) \(pid, iattrs) -> do
                    let i = overAttrs (const iattrs) $ lookupInvestigator (toId iattrs) pid
                    push $ SetInvestigator pid i
              let newAttrs = fromJustNote "not full campaign" (otherCampaignAttrs meta)
              pure
                $ TheDreamEaters
                  ( newAttrs
                      { campaignCompletedSteps = campaignCompletedSteps attrs
                      , campaignStep = s
                      , campaignLog = campaignLog attrs
                      , campaignResolutions = campaignResolutions attrs
                      , campaignModifiers = campaignModifiers attrs
                      }
                      `with` meta
                        { currentCampaignMode = Just TheWebOfDreams
                        , otherCampaignAttrs = Just attrs
                        , currentCampaignPlayers = otherCampaignPlayers meta
                        , otherCampaignPlayers = mapFromList currentPlayers
                        }
                  )
            else pure $ TheDreamEaters (attrs `with` meta)
        _ -> error $ "Unknown scenario: " <> show s
    CampaignStep (InterludeStep 1 _) -> do
      case campaignMode meta of
        PartialMode TheWebOfDreams -> push $ CampaignStep (InterludeStepPart 1 Nothing 3)
        _ -> push $ CampaignStep (InterludeStepPart 1 Nothing 1)
      pure c
    CampaignStep (InterludeStepPart 1 _ 1) -> do
      case campaignMode meta of
        FullMode -> push $ CampaignStep (InterludeStepPart 1 Nothing 2)
        _ -> push $ NextCampaignStep (Just TheSearchForKadath)
      pure c
    CampaignStep (InterludeStepPart 1 _ 2) -> do
      push $ CampaignStep (InterludeStepPart 1 Nothing 3)
      pure c
    CampaignStep (InterludeStepPart 1 _ 3) -> do
      case campaignMode meta of
        FullMode -> do
          lead <- getLeadPlayer
          push
            $ questionLabel "Proceed to which scenario" lead
            $ ChooseOne
              [ Label "The Search for Kadath" [NextCampaignStep (Just TheSearchForKadath)]
              , Label "A Thousand Shapes of Horror" [NextCampaignStep (Just AThousandShapesOfHorror)]
              ]
        _ -> push $ NextCampaignStep (Just AThousandShapesOfHorror)
      pure c
    _ -> defaultCampaignRunner msg c
