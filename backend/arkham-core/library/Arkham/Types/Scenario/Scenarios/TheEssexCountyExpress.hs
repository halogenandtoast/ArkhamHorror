module Arkham.Types.Scenario.Scenarios.TheEssexCountyExpress
  ( TheEssexCountyExpress(..)
  , theEssexCountyExpress
  ) where

import Arkham.Import hiding (Cultist)

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token
import Data.List.NonEmpty (NonEmpty(..))

newtype TheEssexCountyExpress = TheEssexCountyExpress Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theEssexCountyExpress :: Difficulty -> TheEssexCountyExpress
theEssexCountyExpress difficulty = TheEssexCountyExpress $ base
  { scenarioLocationLayout = Just
    ["trainCar6 trainCar5 trainCar4 trainCar3 trainCar2 trainCar1 engineCar"]
  }
 where
  base = baseAttrs
    "02159"
    "The Essex County Express"
    ["02160", "02161", "02162", "02163", "02164"]
    ["02165", "02166"]
    difficulty

theEssexCountyExpressIntro :: Message
theEssexCountyExpressIntro = FlavorText
  (Just "Scenario III: The Essex County Express")
  [ "Recent events in the Museum have forced you to\
    \ re-evaluate Armitage’s tale about Dunwich. It\
    \ cannot be a coincidence—Wilbur Whateley, the\
    \ Necronomicon, the creature from Dunwich, and\
    \ the people and creatures who attacked here in\
    \ Arkham—everything must be connected. You’re\
    \ certain now where you must head: the lonely\
    \ and dismal town of Dunwich Village."
  , "You consider telling the Massachusetts State Police what you know, but\
    \ the negative consequences outweigh the potential gain. Given the nature\
    \ of your story, they would likely write your stories off as an absurd hoax.\
    \ Worse, they could lock you up. After all, you were present in an illegal\
    \ speakeasy, and you also trespassed in the museum. Instead, you decide\
    \ to head to Dunwich yourself, in order to investigate further."
  , "You pack everything you think you might need and manage to get\
    \ some rest for the night. In the morning, you head to the train station in\
    \ Northside and purchase a last-minute express ticket. Dunwich is several\
    \ hours by train northwest along the Miskatonic River Valley. There is no\
    \ train station in Dunwich, but you manage to phone one of Armitage’s\
    \ acquaintances in the small village: a man by the name of Zebulon\
    \ Whateley who was present during the events several months ago."
  , "Armitage’s notes indicate that the Whateley family is spread across\
    \ many branches, some decadent and unscrupulous, others “undecayed”\
    \ or otherwise untouched by nefarious and diabolic rites. According to\
    \ Armitage, Zebulon’s branch of the family lay somewhere between the\
    \ decayed and undecayed Whateleys, who knew of the traditions of his\
    \ ancestors, but was not corrupted by them. He agrees to pick you up at\
    \ the closest station and drive you into town."
  , "As the train departs from Arkham, you feel the events of the previous\
    \ night catching up to you, and exhaustion sets in. But before you can\
    \ safely reach your destination, the train car suddenly rumbles and\
    \ shakes, startling you out of your reverie. The train loudly skids to a\
    \ violent halt, and you hear a rattling noise behind you…"
  ]

instance
  ( HasTokenValue env InvestigatorId
  , HasStep AgendaStep env
  )
  => HasTokenValue env TheEssexCountyExpress where
  getTokenValue (TheEssexCountyExpress attrs) iid = \case
    Skull -> do
      step <- unAgendaStep . getStep <$> ask
      pure $ toTokenValue attrs Skull step (step + 1)
    Cultist -> pure $ toTokenValue attrs Cultist 1 0
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 3 3
    otherFace -> getTokenValue attrs iid otherFace

standaloneTokens :: [Token]
standaloneTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance ScenarioRunner env => RunMessage env TheEssexCountyExpress where
  runMessage msg s@(TheEssexCountyExpress attrs@Attrs {..}) = case msg of
    SetTokensForScenario -> do
      standalone <- isNothing <$> getId @(Maybe CampaignId) ()
      s <$ if standalone
        then unshiftMessage (SetTokens standaloneTokens)
        else pure ()
    Setup -> do
      _standalone <- isNothing <$> getId @(Maybe CampaignId) ()
      investigatorIds <- getInvestigatorIds

      engineCar <- sample $ "02175" :| ["02176", "02177"]
      trainCars <- take 6 <$> shuffleM
        ["02167", "02168", "02169", "02170", "02171", "02172", "02173", "02174"]

      let start = fromJustNote "No train cars?" $ headMay trainCars

      encounterDeck <- buildEncounterDeck
        [ EncounterSet.TheEssexCountyExpress
        , EncounterSet.TheBeyond
        , EncounterSet.StrikingFear
        , EncounterSet.AncientEvils
        , EncounterSet.DarkCult
        ]

      pushMessages
        $ [ story investigatorIds theEssexCountyExpressIntro
          , SetEncounterDeck encounterDeck
          , AddAgenda "02160"
          , AddAct "02165"
          ]
        <> concat
             [ [ PlaceLocation location
               , SetLocationLabel location ("trainCar" <> tshow @Int n)
               ]
             | (n, location) <- zip [6, 5 ..] trainCars
             ]
        <> [RevealLocation Nothing start, MoveAllTo start]

      let
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocation))
          (engineCar : trainCars)
      TheEssexCountyExpress
        <$> runMessage msg (attrs & locationsL .~ locations')
    ResolveToken _ Tablet iid | isEasyStandard attrs ->
      s <$ unshiftMessage (InvestigatorPlaceCluesOnLocation iid 1)
    ResolveToken _ Tablet iid | isHardExpert attrs -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList @EnemyId lid
      mHuntingHorrorId <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorrorId of
        Just huntingHorrorId -> s <$ when
          (huntingHorrorId `elem` enemyIds)
          (unshiftMessage $ EnemyAttack iid huntingHorrorId)
        Nothing -> pure s
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _ ->
      s <$ case drawnTokenFace token of
        Cultist -> unshiftMessage $ FindEncounterCard
          iid
          (toTarget attrs)
          (EncounterCardMatchByCardCode "02141")
        ElderThing -> unshiftMessage $ ChooseAndDiscardAsset iid
        _ -> pure ()
    NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "Whatever the creature in the\
                   \ museum was, you had neither the will nor the tools to destroy\
                   \ it. It seems you must give up any hope of recovering the\
                   \ Necronomicon. Even so, there are others depending on you.\
                   \ Gathering your courage, you prepare for your next task."
                 ]
               ]
           ]
         , Record TheInvestigatorsFailedToRecoverTheNecronomicon
         ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    Resolution 1 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "As long as this translation of the\
                   \ Necronomicon exists, there will be sorcerers and other foul\
                   \ agents like Whateley seeking it. In the end, you know what\
                   \ must be done to protect humanity from the threats you’ve seen.\
                   \ You find a trash bin and fill it with books and documents,\
                   \ throwing the Necronomicon on top. It takes several matches\
                   \ to set the contents of the bin alight. The flames fill the room\
                   \ with heat and the creeping shadows retreat from its light. You\
                   \ watch the book burn for some time, its pages turning to ash.\
                   \ You can only hope you’ve made the right decision."
                 ]
               ]
           ]
         , Record TheInvestigatorsDestroyedTheNecronomicon
         ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    Resolution 2 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 Nothing
                 [ "The Necronomicon is more than just a book;\
                   \ it is a tool. Within its pages is a wealth of information about\
                   \ the forces and creatures you have encountered. Knowing how\
                   \ useful it could be in your endeavors, how could you possibly\
                   \ bring yourself to destroy it? Besides, as long as you keep the\
                   \ book safely in your possession, you will still be foiling those\
                   \ who wish to use it for nefarious purposes."
                 ]
               ]
           ]
         , Record TheInvestigatorsTookCustodyOfTheNecronomicon
         , chooseOne
           leadInvestigatorId
           [ Label
             "Add The Necronomicon (Olaus Wormius Translation) to a deck"
             [ chooseOne
                 leadInvestigatorId
                 [ TargetLabel
                     (InvestigatorTarget iid)
                     [AddCampaignCardToDeck iid "02140"]
                 | iid <- investigatorIds
                 ]
             ]
           , Label
             "Do not add The Necronomicon (Olaus Wormius Translation) to a deck"
             []
           ]
         , AddToken ElderThing
         ]
        <> [ GainXP iid xp | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    _ -> TheEssexCountyExpress <$> runMessage msg attrs
