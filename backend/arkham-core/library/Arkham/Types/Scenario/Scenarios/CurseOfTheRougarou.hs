module Arkham.Types.Scenario.Scenarios.CurseOfTheRougarou
  ( CurseOfTheRougarou(..)
  , curseOfTheRougarou
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait hiding (Cultist)
import Control.Monad.Extra (findM)
import Data.Maybe (fromJust)

newtype CurseOfTheRougarouMetadata = CurseOfTheRougarouMetadata { setAsideLocationTraits :: HashSet Trait }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

newtype CurseOfTheRougarou = CurseOfTheRougarou (ScenarioAttrs `With` CurseOfTheRougarouMetadata)
  deriving stock Generic
  deriving anyclass HasRecord
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

curseOfTheRougarou :: Difficulty -> CurseOfTheRougarou
curseOfTheRougarou difficulty =
  CurseOfTheRougarou
    . (`with` CurseOfTheRougarouMetadata mempty)
    $ (baseAttrs
        "81001"
        "Curse of the Rougarou"
        ["81002", "81003", "81004"]
        ["81005", "81006"]
        difficulty
      )
        { scenarioLocationLayout = Just
          [ "     .       unhallowed1      newOrleans1       ."
          , "unhallowed2 unhallowedBayou newOrleansBayou newOrleans2"
          , "riverside2 riversideBayou wildernessBayou wilderness2"
          , "     .       riverside1      wilderness1       ."
          ]
        }

locationsByTrait :: HashMap Trait [CardCode]
locationsByTrait = mapFromList
  [ (NewOrleans, ["81007", "81008", "81009"])
  , (Riverside, ["81010", "81011", "81012"])
  , (Wilderness, ["81013", "81014", "81015"])
  , (Unhallowed, ["81016", "81017", "81018"])
  ]

locationsWithLabels :: MonadRandom m => Trait -> m [(Text, CardCode)]
locationsWithLabels trait = do
  shuffled <- shuffleM (before <> after)
  pure $ zip labels (bayou : shuffled)
 where
  locationSet = findWithDefault [] trait locationsByTrait
  labels =
    [ pack (camelCase $ show trait) <> "Bayou"
    , pack (camelCase $ show trait) <> "1"
    , pack (camelCase $ show trait) <> "2"
    ]
  (before, bayou : after) =
    break (elem Bayou . toTraits . lookupLocationStub) locationSet

instance (HasTokenValue env InvestigatorId, HasSet Trait env LocationId, HasId LocationId env InvestigatorId) => HasTokenValue env CurseOfTheRougarou where
  getTokenValue (CurseOfTheRougarou (attrs `With` _)) iid = \case
    Skull -> do
      lid <- getId @LocationId iid
      isBayou <- member Bayou <$> getSet lid
      pure $ if isBayou
        then toTokenValue attrs Skull 4 6
        else toTokenValue attrs Skull 2 3
    Cultist -> pure $ toTokenValue attrs Cultist 2 3
    Tablet -> pure $ TokenValue Tablet ZeroModifier
    ElderThing -> pure $ TokenValue ElderThing (NegativeModifier 4)
    otherFace -> getTokenValue attrs iid otherFace

instance (HasId (Maybe LocationId) env LocationMatcher, ScenarioRunner env) => RunMessage env CurseOfTheRougarou where
  runMessage msg s@(CurseOfTheRougarou (attrs `With` metadata)) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      encounterDeck <- buildEncounterDeck [EncounterSet.TheBayou]
      result <- shuffleM $ keys locationsByTrait
      let
        trait = fromJust . headMay . drop 1 $ result
        rest = drop 2 result
      startingLocationsWithLabel <-
        zip <$> getRandoms <*> locationsWithLabels trait
      let
        (_, (bayouId, _) : _) = break
          (elem Bayou . toTraits . lookupLocationStub . snd . snd)
          startingLocationsWithLabel
      pushMessages
        $ [SetEncounterDeck encounterDeck, AddAgenda "81002", AddAct "81005"]
        <> concat
             [ [ PlaceLocation cardCode locationId
               , SetLocationLabel locationId label
               ]
             | (locationId, (label, cardCode)) <- startingLocationsWithLabel
             ]
        <> [ RevealLocation Nothing bayouId
           , MoveAllTo bayouId
           , AskMap
           . mapFromList
           $ [ ( iid
               , ChooseOne
                 [ Run
                     [ Continue "Continue"
                     , FlavorText
                       (Just "Terror Grips New Orleans!")
                       [ "Minnie Klein, your contact at the Arkham Advertiser, has slipped\
                      \ you a draft of the article over a cup of coffee at Velma's Diner. It\
                      \ would have gone to print had Doyle Jeffries, the lead editor, not\
                      \ scoffed at the concept. \"I believe his exact words were, 'I ain't\
                      \ printing the ravings of some Voodoo lunatic and passing is as news,'\"\
                      \ she explained. From the sly grin spreading across her face, you could\
                      \ tell she smelled a story."
                       , "The headline was sensationalist. Three killings, in nine days was\
                      \ enough to spook a town, sure. But you doubt all of New Orleans is\
                      \ gripped by terror, or even knows about the killings. Still, something\
                      \ piqued your interest. \"Lady Esprit,\" the Voodoo priestess from the\
                      \ article, claimed that a malign curse had taken root in the bayou."
                       , "\"There's something to this, isn't there? I know that look,\"\
                      \ Minnie said. You weren't sure. If Lady Esprit was right, this\
                      \ \"roux-ga-roux\" wouldn't stop killing at three, that's for sure.\
                      \ But curses? Wolf-people? How could such things be real? Only one way\
                      \ to find out. You put on your coat and head for the Northside Station..."
                       ]
                     ]
                 ]
               )
             | iid <- investigatorIds
             ]
           ]

      let
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocationStub))
          [ "81007"
          , "81008"
          , "81009"
          , "81010"
          , "81011"
          , "81012"
          , "81013"
          , "81014"
          , "81015"
          , "81016"
          , "81017"
          , "81018"
          ]
      CurseOfTheRougarou
        . (`with` metadata { setAsideLocationTraits = setFromList rest })
        <$> runMessage msg (attrs & locationsL .~ locations')
    PutSetAsideIntoPlay (SetAsideLocationsTarget _) -> do
      setAsideLocationsWithLabels <- concat <$> traverse
        locationsWithLabels
        (setToList $ setAsideLocationTraits metadata)
      locationIds <- getRandoms
      unshiftMessages $ concat
        [ [PlaceLocation cardCode locationId, SetLocationLabel locationId label]
        | (locationId, (label, cardCode)) <- zip
          locationIds
          setAsideLocationsWithLabels
        ]
      CurseOfTheRougarou
        . (`with` metadata { setAsideLocationTraits = mempty })
        <$> runMessage msg attrs
    SetTokensForScenario -> do
      let
        tokens = if isEasyStandard attrs
          then
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
            , MinusThree
            , MinusThree
            , MinusFour
            , MinusFour
            , MinusFive
            , MinusSix
            , Skull
            , Skull
            , Cultist
            , Cultist
            , Tablet
            , ElderThing
            , AutoFail
            , ElderSign
            ]
          else
            [ PlusOne
            , Zero
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
            , MinusFour
            , MinusFive
            , MinusFive
            , MinusSix
            , MinusEight
            , Skull
            , Skull
            , Skull
            , Cultist
            , Cultist
            , Tablet
            , ElderThing
            , AutoFail
            , ElderSign
            ]
      s <$ unshiftMessage (SetTokens tokens)
    ResolveToken _ Cultist iid -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList @EnemyId lid
      rougarouAtYourLocation <- elem "81028" <$> for enemyIds (getId @CardCode)
      s <$ when rougarouAtYourLocation (unshiftMessage $ DrawAnotherToken iid)
    ResolveToken _ ElderThing iid -> if isEasyStandard attrs
      then do
        lid <- getId @LocationId iid
        enemyIds <- getSetList @EnemyId lid
        mrougarou <- findM (((== "81028") <$>) . getId @CardCode) enemyIds
        s <$ for_ mrougarou (unshiftMessage . EnemyWillAttack iid)
      else do
        lid <- getId @LocationId iid
        connectedLocationIds <- map unConnectedLocationId <$> getSetList lid
        enemyIds <- concat
          <$> for (lid : connectedLocationIds) (getSetList @EnemyId)
        mrougarou <- findM (((== "81028") <$>) . getId @CardCode) enemyIds
        s <$ for_ mrougarou (unshiftMessage . EnemyWillAttack iid)
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _ _ -> s <$ when
      (drawnTokenFace token == Tablet)
      (unshiftMessage $ CreateEffect
        "81001"
        Nothing
        (DrawnTokenSource token)
        (InvestigatorTarget iid)
      )
    ScenarioResolution NoResolution ->
      runMessage (ScenarioResolution $ Resolution 1) s
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "Somehow, you manage to make it back safely before daybreak,\
                    \ resting until late in the afternoon. It isn't until you seek\
                    \ out Lady Esprit the next day that you realize who last night's\
                    \ victim was. With a heavy heart and an unshakable dread, you\
                    \ choose to bury her body instead of contacting the authorities\
                    \—the less people who delve this deep into the bayou, the better."
                ]
              , Record TheRougarouContinuesToHauntTheBayou
              ]
            <> [ GainXP iid xp | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "The creature gives a pitiful wail as dark miry blood oozes from\
                  \ its wounds. By the time its body collapses into the mud, it has\
                  \ transformed back into its original form—the form of a yound dark\
                  \-skinned man, his expression twisted in agony. You bring his body\
                  \ back to Lady Esprit and she works her strange magic, removing the\
                  \ stain of the curse from the land. \"Call on me should you ever\
                  \ need my help,\" the mysterious woman tells you."
                ]
              , Record TheRougarouIsDestroyed
              , RemoveCampaignCardFromDeck leadInvestigatorId "81029"
              , chooseOne
                leadInvestigatorId
                [ Label
                  "Add Lady Esprit to your deck"
                  [AddCampaignCardToDeck leadInvestigatorId "81019"]
                , Label "Do not add Lady Esprit to your deck" []
                ]
              ]
            <> [ GainXP iid xp | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    ScenarioResolution (Resolution 3) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "Somehow, you have managed to quell the rage and bloodlust\
                  \ of the curse within the creature, and in moments the shape\
                  \ of a young, dark-skinned man stands before you, panting and\
                  \ sweating. He seems to onky just now understand everything\
                  \ he's done, and agrees to flee to a secluded corner of the\
                  \ earth where he can harm no one. However, the curse lives\
                  \ on. He sees it in your eyes and grips your arm tightly.\
                  \\"Don't let it take control,\" he warns. \"I was weak, but\
                  \ you—I can tell you are strong. Control the curse as I could\
                  \ not.\""
                ]
              , Record TheRougarouEscapedAndYouEmbracedTheCurse
              , AddCampaignCardToDeck leadInvestigatorId "81030"
              ]
            <> [ GainXP iid xp | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    _ -> CurseOfTheRougarou . (`with` metadata) <$> runMessage msg attrs
