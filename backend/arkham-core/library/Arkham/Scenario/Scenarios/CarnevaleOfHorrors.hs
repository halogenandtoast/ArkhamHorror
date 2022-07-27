module Arkham.Scenario.Scenarios.CarnevaleOfHorrors
  ( CarnevaleOfHorrors(..)
  , carnevaleOfHorrors
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Direction
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait hiding ( Cultist )
import Data.List.NonEmpty qualified as NE

newtype CarnevaleOfHorrors = CarnevaleOfHorrors ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

carnevaleOfHorrors :: Difficulty -> CarnevaleOfHorrors
carnevaleOfHorrors difficulty = scenario
  CarnevaleOfHorrors
  "82001"
  "Carnevale of Horrors"
  difficulty
  [ ".         .         .         location1  .         .         ."
  , ".         location8 location8 location1  location2 location2 ."
  , ".         location8 location8 .          location2 location2 ."
  , "location7 location7 .         cnidathqua gondola   location3 location3"
  , ".         location6 location6 .          location4 location4 ."
  , ".         location6 location6 location5  location4 location4 ."
  , ".         .         .         location5  .         .         ."
  ]

instance HasTokenValue CarnevaleOfHorrors where
  getTokenValue iid tokenFace (CarnevaleOfHorrors attrs) = case tokenFace of
    Skull -> do
      let
        countInnocentRevelers = count ((== Assets.innocentReveler) . toCardDef)
        innocentRevelerCount =
          countInnocentRevelers (scenarioCardsUnderAgendaDeck attrs)
            + (if isEasyStandard attrs
                then 0
                else countInnocentRevelers (scenarioCardsUnderActDeck attrs)
              )
      pure $ TokenValue Skull (NegativeModifier $ 2 + innocentRevelerCount)
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ toTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 4 6
    otherFace -> getTokenValue iid otherFace attrs

masks :: [CardDef]
masks =
  [Assets.pantalone, Assets.medicoDellaPeste, Assets.bauta, Assets.gildedVolto]

sacrificesMade
  :: InvestigatorId -> [InvestigatorId] -> ScenarioAttrs -> [Message]
sacrificesMade leadInvestigatorId investigatorIds s =
  chooseOne
      leadInvestigatorId
      [ Run
          [ Continue "Continue"
          , FlavorText
            Nothing
            [ "Too many lives were lost during the eclipse to stop the machinations\
                \ of Cindathqua's servants. The beast has been fed, its minions empowered.\
                \ You find yourself hoping you don\'t live long enough to see the fallout\
                \ of your failure."
            ]
          ]
      ]
    : [ SearchCollectionForRandom
          iid
          (toSource s)
          (CardWithType PlayerTreacheryType
          <> CardWithOneOf (map CardWithTrait [Madness, Injury, Monster])
          )
      | iid <- investigatorIds
      ]

abbessSatisfied :: InvestigatorId -> [InvestigatorId] -> [Message]
abbessSatisfied leadInvestigatorId investigatorIds =
  chooseOne
      leadInvestigatorId
      [ Run
          [ Continue "Continue"
          , FlavorText
            Nothing
            [ "\"Grazie mille - thank you for all your help,\" Allegria says as you return\
            \ to the basilica. \"Thanks to you, there were few casualties. I shudder to think\
            \ what might have happened had you not arrived. Should you ever require assistance,\
            \ please do not hesitate to ask."
            ]
          ]
      ]
    : [ addCampaignCardToDeckChoice
          leadInvestigatorId
          investigatorIds
          Assets.abbessAllegriaDiBiase
      ]

additionalRewards :: (Monad m, HasGame m) => ScenarioAttrs -> m [Message]
additionalRewards s = do
  leadInvestigatorId <- getLeadInvestigatorId
  investigatorIds <- getInvestigatorIds
  let
    proceedToSacrificesMade =
      if null (scenarioCardsUnderActDeck s)
          && notNull (scenarioCardsUnderAgendaDeck s)
        then sacrificesMade leadInvestigatorId investigatorIds s
        else []
    proceedToAbbessSatisfied =
      if null (scenarioCardsUnderAgendaDeck s)
          && length (scenarioCardsUnderAgendaDeck s)
          == 3
        then abbessSatisfied leadInvestigatorId investigatorIds
        else []
  pure
    $ [ChooseOneRewardByEachPlayer masks investigatorIds]
    <> proceedToSacrificesMade
    <> proceedToAbbessSatisfied

instance RunMessage CarnevaleOfHorrors where
  runMessage msg s@(CarnevaleOfHorrors attrs) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds

      -- Encounter Deck
      encounterDeck <- buildEncounterDeckExcluding
        [ Enemies.donLagorio
        , Enemies.elisabettaMagro
        , Enemies.salvatoreNeri
        , Enemies.savioCorvi
        , Enemies.cnidathqua
        ]
        [EncounterSet.CarnevaleOfHorrors]

      -- Locations
      let locationLabels = [ "location" <> tshow @Int n | n <- [1 .. 8] ]
      randomLocations <- traverse genCard . drop 1 =<< shuffleM
        [ Locations.streetsOfVenice
        , Locations.rialtoBridge
        , Locations.venetianGarden
        , Locations.bridgeOfSighs
        , Locations.floodedSquare
        , Locations.accademiaBridge
        , Locations.theGuardian
        ]
      canalSide <- genCard Locations.canalSide
      sanMarcoBasilica <- genCard Locations.sanMarcoBasilica

      let
        unshuffled = canalSide : randomLocations
        nonSanMarcoBasilicaLocationIds = map (LocationId . toCardId) unshuffled
        sanMarcoBasilicaId = LocationId $ toCardId sanMarcoBasilica

      locationIdsWithMaskedCarnevaleGoers <-
        zip nonSanMarcoBasilicaLocationIds
          <$> (shuffleM =<< traverse
                genCard
                [ Assets.maskedCarnevaleGoer_17
                , Assets.maskedCarnevaleGoer_18
                , Assets.maskedCarnevaleGoer_19
                , Assets.maskedCarnevaleGoer_20
                , Assets.maskedCarnevaleGoer_21
                , Assets.maskedCarnevaleGoer_21
                , Assets.maskedCarnevaleGoer_21
                ]
              )
      locations <- (sanMarcoBasilica :|) <$> shuffleM unshuffled

      -- Assets
      abbess <- genCard Assets.abbessAllegriaDiBiase

      pushAllEnd
        $ [SetEncounterDeck encounterDeck, SetAgendaDeck, SetActDeck]
        <> [ PlaceLocation cardDef | cardDef <- toList locations ]
        <> [ SetLocationLabel (LocationId $ toCardId location) label
           | (label, location) <- zip locationLabels (toList locations)
           ]
        <> [ PlacedLocationDirection
               (LocationId $ toCardId l1)
               RightOf
               (LocationId $ toCardId l2)
           | (l1, l2) <- zip (toList locations) (drop 1 $ toList locations)
           ]
        <> [ PlacedLocationDirection
               (LocationId . toCardId $ NE.last locations)
               RightOf
               (LocationId . toCardId $ NE.head locations)
           ]
        <> [ CreateAssetAt asset (AtLocation locationId)
           | (locationId, asset) <- locationIdsWithMaskedCarnevaleGoers
           ]
        <> [ CreateAssetAt abbess (AtLocation sanMarcoBasilicaId)
           , RevealLocation Nothing sanMarcoBasilicaId
           , MoveAllTo (toSource attrs) sanMarcoBasilicaId
           , AskMap
           . mapFromList
           $ [ ( iid
               , ChooseOne
                 [ Run
                     [ Continue "Continue"
                     , FlavorText
                       (Just "The Carnevale is Coming...")
                       [ "\"Look,\" Sheriff Engel insists, \"I know it sounds crazy, but that's\
                       \ all there is to it.\" He sighs and sits back down, pouring a cup of joe\
                       \ for you and one for himself. \"A dame in Uptown spotted a cracked egg\
                       \ wearing this mask and holdin' a bloody butcher's cleaver,\" he says,\
                       \ motioning to the black leather mask sitting on his desk. It has a comically\
                       \ long nose and a strange symbol scrawled in yellow on its forehead. \"So, she\
                       \ calls it in. My boys and I picked him up on the corner of Saltonstall &\
                       \ Garrison.\" The sheriff\'s jaw clenches and his brows furrow as he recounts\
                       \ the story. \"Fella did nothing but laugh as we slapped the bracelets on him.\
                       \ Called himself Zanni. Said nothing except the 'carnival is coming,' whatever\
                       \ the hell that meant. Wasn't until the next day we found the victim's body.\
                       \ Defense wanted him in a straitjacket. We were happy to oblige.\""
                       , "There isn't much time to spare. If your research is right, there is more to\
                       \ this case than meets the eye. This \"Zanni\" wasn't talking about Darke's\
                       \ Carnival, but rather, the Carnevale of Venice, which begins just before the\
                       \ next full moon..."
                       ]
                     ]
                 ]
               )
             | iid <- investigatorIds
             ]
           ]


      setAsideCards <- traverse
        genCard
        [ Enemies.cnidathqua
        , Assets.pantalone
        , Assets.medicoDellaPeste
        , Assets.bauta
        , Assets.gildedVolto
        ]

      CarnevaleOfHorrors <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [Acts.theCarnevaleConspiracy, Acts.getToTheBoats, Acts.row]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.theFestivitiesBegin
             , Agendas.theShadowOfTheEclipse
             , Agendas.chaosAtTheCarnevale
             ]
          )
        )
    SetTokensForScenario -> do
      let
        tokens = if isEasyStandard attrs
          then
            [ PlusOne
            , Zero
            , Zero
            , Zero
            , MinusOne
            , MinusOne
            , MinusOne
            , MinusTwo
            , MinusThree
            , MinusFour
            , MinusSix
            , Skull
            , Skull
            , Skull
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
            , MinusThree
            , MinusFour
            , MinusFive
            , MinusSix
            , MinusSeven
            , Skull
            , Skull
            , Skull
            , Cultist
            , Tablet
            , ElderThing
            , AutoFail
            , ElderSign
            ]
      s <$ push (SetTokens tokens)
    ResolveToken _ Cultist iid -> s <$ push (DrawAnotherToken iid)
    ResolveToken token Tablet iid | isHardExpert attrs -> do
      lid <- getJustLocation iid
      closestInnocentRevelers <- selectList $ ClosestAsset lid $ assetIs
        Assets.innocentReveler
      case closestInnocentRevelers of
        [] -> pure ()
        [x] -> push
          (chooseOne
            iid
            [ AssetDamage x (TokenSource token) 1 0
            , AssetDamage x (TokenSource token) 0 1
            ]
          )
        xs -> push
          (chooseOne
            iid
            [ TargetLabel
                (AssetTarget x)
                [ chooseOne
                    iid
                    [ AssetDamage x (TokenSource token) 1 0
                    , AssetDamage x (TokenSource token) 0 1
                    ]
                ]
            | x <- xs
            ]
          )
      pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> do
      case tokenFace token of
        Cultist -> push $ InvestigatorDrawEncounterCard iid
        Tablet -> do
          lid <- getJustLocation iid
          closestInnocentRevelers <- selectList $ ClosestAsset lid $ assetIs
            Assets.innocentReveler
          case closestInnocentRevelers of
            [] -> pure ()
            [x] -> push $ chooseOne
              iid
              [ AssetDamage x (TokenSource token) 1 0
              , AssetDamage x (TokenSource token) 0 1
              ]
            xs -> push $ chooseOne
              iid
              [ targetLabel
                  x
                  [ chooseOne
                      iid
                      [ AssetDamage x (TokenSource token) 1 0
                      , AssetDamage x (TokenSource token) 0 1
                      ]
                  ]
              | x <- xs
              ]
        ElderThing -> do
          mCnidathquaId <- getCnidathqua
          case mCnidathquaId of
            Just cnidathquaId -> push $ EnemyAttack
              iid
              cnidathquaId
              (DamageFirst Assets.innocentReveler)
              RegularAttack
            Nothing -> pure ()
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      additionalRewardsMsg <- additionalRewards
        (attrs
        & (cardsUnderActDeckL %~ drop 1)
        & (cardsUnderAgendaDeckL <>~ take 1 (scenarioCardsUnderActDeck attrs))
        )
      s <$ pushAll
        ([ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "You sputter awake as an oar gently taps your shoulder. \"Tutto bene?\"\
                     \ The gondolier holding the oar says with a concerned expression. You nod\
                     \ and drag yourself onto the docks from his gondola, drenched and aching\
                     \ all over. The city is devastated. Most of the boats in the canal are\
                     \  wrecked, and the streets are covered not in confetti, but in blood..."
                   ]
                 , Record ManyWereSacrificedToCnidathquaDuringTheCarnivale
                 ]
             ]
         ]
        <> additionalRewardsMsg
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      additionalRewardsMsg <- additionalRewards attrs
      s <$ pushAll
        ([ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   (Just "Resolution 1")
                   [ "The city is still recovering from the events during the eclipse. With\
                     \ nearly all evidence of the creature melted away by the hot sun, many\
                     \ attribute the violence during the Carnevale to local crime lord Cascio\
                     \ Di Boerio and his crew. Those that know the truth know better than the\
                     \ speak of the elder creature that lives in Laguna Veneta. With any luck,\
                     \ its name will never be spoken again."
                   ]
                 , Record TheSunBanishedCnidathquaIntoTheDepths
                 ]
             ]
         ]
        <> additionalRewardsMsg
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      additionalRewardsMsg <- additionalRewards attrs
      s <$ pushAll
        ([ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   (Just "Resolution 2")
                   [ "The creature recoils as globules of its jelly-like flesh rip and tear\
                     \ from its body, splashing into the lagoon. It makes no sound as its torn\
                     \ body sinks into the depths. The chanting in the city plunges into mournful\
                     \ silence. As you return it to the canal-side streets, black feathers fall\
                     \ from the sky where bright confetti once fluttered. You can only wonder how\
                     \ long it will take for the creature to recover."
                   ]
                 , Record CnidathquaRetreatedToNurseItsWounds
                 ]
             ]
         ]
        <> additionalRewardsMsg
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ChooseOneRewardByEachPlayer rewards@(_ : _) (currentInvestigatorId : rest)
      -> do
        s <$ push
          (chooseOne
            currentInvestigatorId
            (Label
                "Do not add a mask"
                [ChooseOneRewardByEachPlayer rewards rest]
            : [ CardLabel
                  (toCardCode reward)
                  [ AddCampaignCardToDeck currentInvestigatorId reward
                  , ChooseOneRewardByEachPlayer (delete reward rewards) rest
                  ]
              | reward <- rewards
              ]
            )
          )
    _ -> CarnevaleOfHorrors <$> runMessage msg attrs
