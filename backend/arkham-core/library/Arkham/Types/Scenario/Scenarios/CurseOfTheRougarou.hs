{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.CurseOfTheRougarou
  ( CurseOfTheRougarou(..)
  , curseOfTheRougarou
  )
where

import Arkham.Import

import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Helpers
import Arkham.Types.Location
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import qualified Arkham.Types.Token as Token
import Arkham.Types.Trait
import Control.Monad.Extra (findM)
import System.Random.Shuffle

newtype CurseOfTheRougarouMetadata = CurseOfTheRougarouMetadata { setAsideLocationTraits :: HashSet Trait }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CurseOfTheRougarou = CurseOfTheRougarou (Attrs `With` CurseOfTheRougarouMetadata)
  deriving newtype (Show, ToJSON, FromJSON)

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

locations :: HashMap Trait [LocationId]
locations = mapFromList
  [ (NewOrleans, ["81007", "81008", "81009"])
  , (Riverside, ["81010", "81011", "81012"])
  , (Wilderness, ["81013", "81014", "81015"])
  , (Unhallowed, ["81016", "81017", "81018"])
  ]

locationsWithLabels :: MonadIO m => Trait -> m [(Text, LocationId)]
locationsWithLabels trait = do
  shuffled <- liftIO $ shuffleM (before <> after)
  pure $ zip labels (bayou : shuffled)
 where
  locationSet = findWithDefault [] trait locations
  labels =
    [ pack (camelCase $ show trait) <> "Bayou"
    , pack (camelCase $ show trait) <> "1"
    , pack (camelCase $ show trait) <> "2"
    ]
  (before, bayou : after) =
    break (elem Bayou . getTraits . lookupLocation) locationSet

instance (HasTokenValue env InvestigatorId, HasQueue env, HasSet Trait LocationId env, HasId LocationId InvestigatorId env) => HasTokenValue env CurseOfTheRougarou where
  getTokenValue (CurseOfTheRougarou (attrs `With` _)) iid token =
    case drawnTokenFace token of
      Token.Skull -> do
        lid <- asks $ getId @LocationId iid
        isBayou <- asks $ member Bayou . getSet lid
        let
          tokenVal
            | isBayou = if isEasyStandard attrs then 4 else 6
            | otherwise = if isEasyStandard attrs then 2 else 3
        pure $ TokenValue token (NegativeModifier tokenVal)
      Token.Cultist -> pure $ TokenValue
        token
        (NegativeModifier $ if isEasyStandard attrs then 2 else 3)
      Token.Tablet -> pure $ TokenValue token (PositiveModifier 0)
      Token.ElderThing -> pure $ TokenValue token (NegativeModifier 4)
      _other -> getTokenValue attrs iid token

instance ScenarioRunner env => RunMessage env CurseOfTheRougarou where
  runMessage msg s@(CurseOfTheRougarou (attrs@Attrs {..} `With` metadata)) =
    case msg of
      InitDeck iid deck -> s <$ unshiftMessage (LoadDeck iid deck)
      StartCampaign -> s <$ unshiftMessages [StartScenario "81001"]
      Setup -> do
        investigatorIds <- getInvestigatorIds
        encounterDeck <- buildEncounterDeck [EncounterSet.TheBayou]
        (_ : trait : rest) <- liftIO . shuffleM $ keys locations
        startingLocationsWithLabel <- locationsWithLabels trait
        let
          (_, bayou : _) = break
            (elem Bayou . getTraits . lookupLocation)
            (map snd startingLocationsWithLabel)
        pushMessages
          $ [SetEncounterDeck encounterDeck, AddAgenda "81002", AddAct "81005"]
          <> [ PlaceLocation lid | (_, lid) <- startingLocationsWithLabel ]
          <> [ SetLocationLabel lid label
             | (label, lid) <- startingLocationsWithLabel
             ]
          <> [ RevealLocation bayou
             , MoveAllTo bayou
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
        CurseOfTheRougarou
          . (`with` metadata { setAsideLocationTraits = setFromList rest })
          <$> runMessage msg attrs
      PutSetAsideIntoPlay (SetAsideLocations _) -> do
        setAsideLocationsWithLabels <- concat <$> traverse
          locationsWithLabels
          (setToList $ setAsideLocationTraits metadata)
        unshiftMessages
          $ [ PlaceLocation lid | (_, lid) <- setAsideLocationsWithLabels ]
          <> [ SetLocationLabel lid label
             | (label, lid) <- setAsideLocationsWithLabels
             ]
        CurseOfTheRougarou
          . (`with` metadata { setAsideLocationTraits = mempty })
          <$> runMessage msg attrs
      SetTokensForScenario -> do
        let
          tokens = if isEasyStandard attrs
            then
              [ Token.PlusOne
              , Token.PlusOne
              , Token.Zero
              , Token.Zero
              , Token.Zero
              , Token.MinusOne
              , Token.MinusOne
              , Token.MinusOne
              , Token.MinusTwo
              , Token.MinusTwo
              , Token.MinusThree
              , Token.MinusThree
              , Token.MinusFour
              , Token.MinusFour
              , Token.MinusFive
              , Token.MinusSix
              , Token.Skull
              , Token.Skull
              , Token.Cultist
              , Token.Cultist
              , Token.Tablet
              , Token.ElderThing
              , Token.AutoFail
              , Token.ElderSign
              ]
            else
              [ Token.PlusOne
              , Token.Zero
              , Token.Zero
              , Token.Zero
              , Token.MinusOne
              , Token.MinusOne
              , Token.MinusOne
              , Token.MinusTwo
              , Token.MinusTwo
              , Token.MinusThree
              , Token.MinusThree
              , Token.MinusFour
              , Token.MinusFour
              , Token.MinusFive
              , Token.MinusFive
              , Token.MinusSix
              , Token.MinusEight
              , Token.Skull
              , Token.Skull
              , Token.Skull
              , Token.Cultist
              , Token.Cultist
              , Token.Tablet
              , Token.ElderThing
              , Token.AutoFail
              , Token.ElderSign
              ]
        s <$ unshiftMessage (SetTokens tokens)
      ResolveToken Token.Cultist iid -> do
        lid <- asks $ getId @LocationId iid
        enemyIds <- asks $ setToList . getSet @EnemyId lid
        rougarouAtYourLocation <- elem "81028"
          <$> for enemyIds (asks . getId @CardCode)
        s <$ when rougarouAtYourLocation (unshiftMessage $ DrawAnotherToken iid)
      ResolveToken Token.ElderThing iid -> if isEasyStandard attrs
        then do
          lid <- asks $ getId @LocationId iid
          enemyIds <- asks $ setToList . getSet @EnemyId lid
          mrougarou <- findM
            (asks . ((== "81028") .) . getId @CardCode)
            enemyIds
          s <$ for_ mrougarou (unshiftMessage . EnemyWillAttack iid)
        else do
          lid <- asks $ getId @LocationId iid
          connectedLocationIds <-
            asks $ map unConnectedLocationId . setToList . getSet lid
          enemyIds <- concat <$> for
            (lid : connectedLocationIds)
            (\lid' -> asks $ setToList . getSet @EnemyId lid')
          mrougarou <- findM
            (asks . ((== "81028") .) . getId @CardCode)
            enemyIds
          s <$ for_ mrougarou (unshiftMessage . EnemyWillAttack iid)
      FailedSkillTest iid _ _ (DrawnTokenTarget token) _ -> do
        s <$ when
          (drawnTokenFace token == Token.Tablet)
          (unshiftMessage $ AddModifiers
            (InvestigatorTarget iid)
            (DrawnTokenSource token)
            [CannotMove]
          )
      NoResolution -> runMessage (Resolution 1) s
      Resolution 1 -> do
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
      Resolution 2 -> do
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
                , Ask
                  leadInvestigatorId
                  (ChooseOne
                    [ Label
                      "Add Lady Esprit to your deck"
                      [AddCampaignCardToDeck leadInvestigatorId "81019"]
                    , Label "Do not add Lady Esprit to your deck" []
                    ]
                  )
                ]
              <> [ GainXP iid xp | iid <- investigatorIds ]
              <> [EndOfGame]
            ]
          )
      Resolution 3 -> do
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
