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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheDreamEaters = TheDreamEaters (CampaignAttrs `With` Metadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theDreamEaters :: Difficulty -> TheDreamEaters
theDreamEaters difficulty =
  campaign
    (TheDreamEaters . (`with` Metadata FullMode Nothing Nothing))
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
      pushAll $ map chooseDeck players
      pure
        $ TheDreamEaters
        $ attrs {campaignChaosBag = initChaosBag TheDreamQuest (campaignDifficulty attrs)}
        `with` Metadata (PartialMode TheDreamQuest) Nothing Nothing
    CampaignStep (PrologueStepPart 3) -> do
      players <- allPlayers
      pushAll $ map chooseDeck players
      pure
        $ TheDreamEaters
        $ attrs {campaignChaosBag = initChaosBag TheWebOfDreams (campaignDifficulty attrs)}
        `with` Metadata (PartialMode TheWebOfDreams) Nothing Nothing
    CampaignStep (PrologueStepPart 11) -> do
      players <- allPlayers
      pushAll $ map chooseDeck players <> [NextCampaignStep (Just BeyondTheGatesOfSleep)]
      pure
        $ TheDreamEaters
        $ attrs {campaignChaosBag = initChaosBag TheDreamQuest (campaignDifficulty attrs)}
        `with` Metadata FullMode (Just TheDreamQuest) (Just attrs)
    CampaignStep (PrologueStepPart 12) -> do
      players <- allPlayers
      pushAll $ map chooseDeck players <> [NextCampaignStep (Just WakingNightmare)]
      pure
        $ TheDreamEaters
        $ attrs {campaignChaosBag = initChaosBag TheWebOfDreams (campaignDifficulty attrs)}
        `with` Metadata FullMode (Just TheWebOfDreams) (Just attrs)
    CampaignStep s@(ScenarioStep _) -> do
      void $ defaultCampaignRunner msg c
      case s of
        _ | s `elem` theDreamQuestSteps -> do
          pure
            $ if currentCampaignMode meta == Just TheWebOfDreams
              then
                TheDreamEaters
                  ( fromJustNote "not full campaign" (otherCampaignAttrs meta)
                      `with` Metadata (campaignMode meta) (Just TheDreamQuest) (Just attrs)
                  )
              else TheDreamEaters (attrs `with` meta)
        _ | s `elem` theWebOfDreamsSteps -> do
          pure
            $ if currentCampaignMode meta == Just TheDreamQuest
              then
                TheDreamEaters
                  ( fromJustNote "not full campaign" (otherCampaignAttrs meta)
                      `with` Metadata (campaignMode meta) (Just TheWebOfDreams) (Just attrs)
                  )
              else TheDreamEaters (attrs `with` meta)
        _ -> error $ "Unknown scenario: " <> show s
    _ -> defaultCampaignRunner msg c
