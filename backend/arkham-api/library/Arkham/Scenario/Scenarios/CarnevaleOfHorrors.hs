module Arkham.Scenario.Scenarios.CarnevaleOfHorrors (CarnevaleOfHorrors (..), carnevaleOfHorrors) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message (pattern DealAssetDamage)
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (cardsUnderActDeckL, cardsUnderAgendaDeckL)
import Arkham.Scenarios.CarnevaleOfHorrors.FlavorText qualified as Flavor
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Strategy
import Arkham.Trait hiding (Cultist, ElderThing)
import Data.List.NonEmpty qualified as NE

newtype CarnevaleOfHorrors = CarnevaleOfHorrors ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

carnevaleOfHorrors :: Difficulty -> CarnevaleOfHorrors
carnevaleOfHorrors difficulty =
  sideStory
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

instance HasChaosTokenValue CarnevaleOfHorrors where
  getChaosTokenValue iid chaosTokenFace (CarnevaleOfHorrors attrs) = case chaosTokenFace of
    Skull -> do
      let
        countInnocentRevelers = count ((== Assets.innocentReveler) . toCardDef)
        innocentRevelerCount =
          countInnocentRevelers attrs.cardsUnderAgendaDeck
            + if isEasyStandard attrs then 0 else countInnocentRevelers attrs.cardsUnderActDeck
      pure $ ChaosTokenValue Skull (NegativeModifier $ 2 + innocentRevelerCount)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 6
    otherFace -> getChaosTokenValue iid otherFace attrs

masks :: [CardDef]
masks = [Assets.pantalone, Assets.medicoDellaPeste, Assets.bauta, Assets.gildedVolto]

additionalRewards :: ReverseQueue m => ScenarioAttrs -> m ()
additionalRewards s = do
  investigators <- allInvestigators
  push $ ChooseOneRewardByEachPlayer masks investigators

  when (null s.cardsUnderActDeck && notNull s.cardsUnderAgendaDeck) do
    story Flavor.sacrificesMade
    for_ investigators \iid -> searchCollectionForRandomBasicWeakness iid s [Madness, Injury, Monster]
  when (null s.cardsUnderAgendaDeck && length s.cardsUnderActDeck == 3) do
    story Flavor.abbessSatisfied
    addCampaignCardToDeckChoice investigators Assets.abbessAllegriaDiBiase

instance RunMessage CarnevaleOfHorrors where
  runMessage msg s@(CarnevaleOfHorrors attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story Flavor.intro
      pure s
    Setup -> runScenarioSetup CarnevaleOfHorrors attrs do
      gather Set.CarnevaleOfHorrors

      sanMarcoBasilica <- place Locations.sanMarcoBasilica
      startAt sanMarcoBasilica

      otherLocations <-
        placeAllCapture
          =<< shuffleIn Locations.canalSide
          =<< removeRandom
            [ Locations.streetsOfVenice
            , Locations.rialtoBridge
            , Locations.venetianGarden
            , Locations.bridgeOfSighs
            , Locations.floodedSquare
            , Locations.accademiaBridge
            , Locations.theGuardian
            ]

      let locations = sanMarcoBasilica : otherLocations

      let locationLabels = ["location" <> tshow @Int n | n <- [1 .. 8]]
      for_ (zip locationLabels locations) \(label, location) ->
        push $ SetLocationLabel location label

      for_ (zip locations $ drop 1 locations) \(l1, l2) ->
        push $ PlacedLocationDirection l2 RightOf l1

      push
        $ PlacedLocationDirection sanMarcoBasilica RightOf (NE.last $ sanMarcoBasilica :| otherLocations)

      maskedCarnevaleGoers <-
        shuffleM
          [ Assets.maskedCarnevaleGoer_17
          , Assets.maskedCarnevaleGoer_18
          , Assets.maskedCarnevaleGoer_19
          , Assets.maskedCarnevaleGoer_20
          , Assets.maskedCarnevaleGoer_21
          , Assets.maskedCarnevaleGoer_21
          , Assets.maskedCarnevaleGoer_21
          ]

      for_ (zip maskedCarnevaleGoers otherLocations) (uncurry assetAt_)
      assetAt_ Assets.abbessAllegriaDiBiase sanMarcoBasilica

      setAside
        [ Enemies.cnidathqua
        , Assets.pantalone
        , Assets.medicoDellaPeste
        , Assets.bauta
        , Assets.gildedVolto
        ]

      setAgendaDeck
        [ Agendas.theFestivitiesBegin
        , Agendas.theShadowOfTheEclipse
        , Agendas.chaosAtTheCarnevale
        ]
      setActDeck [Acts.theCarnevaleConspiracy, Acts.getToTheBoats, Acts.row]
    SetChaosTokensForScenario -> do
      setChaosTokens
        $ if isEasyStandard attrs
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
      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken token Tablet iid | isHardExpert attrs -> do
      withLocationOf iid \lid -> do
        closestInnocentRevelers <- select $ ClosestAsset lid $ assetIs Assets.innocentReveler
        chooseTargetM iid closestInnocentRevelers \x ->
          chooseOne
            iid
            [ AssetDamageLabel x [DealAssetDamage x (ChaosTokenSource token) 1 0]
            , AssetHorrorLabel x [DealAssetDamage x (ChaosTokenSource token) 0 1]
            ]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> drawEncounterCard iid Cultist
        Tablet -> do
          withLocationOf iid \lid -> do
            closestInnocentRevelers <- select $ ClosestAsset lid $ assetIs Assets.innocentReveler
            chooseTargetM iid closestInnocentRevelers \x ->
              chooseOne
                iid
                [ AssetDamageLabel x [DealAssetDamage x (ChaosTokenSource token) 1 0]
                , AssetHorrorLabel x [DealAssetDamage x (ChaosTokenSource token) 0 1]
                ]
        ElderThing -> do
          getCnidathqua >>= traverse_ \cnidathqua -> do
            push
              $ EnemyAttack
              $ (enemyAttack cnidathqua attrs iid)
                { attackDamageStrategy = DamageFirst Assets.innocentReveler
                }
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      story Flavor.noResolution
      record ManyWereSacrificedToCnidathquaDuringTheCarnivale
      additionalRewards
        $ attrs
        & (cardsUnderActDeckL %~ drop 1)
        & (cardsUnderAgendaDeckL <>~ take 1 attrs.cardsUnderActDeck)
      allGainXp attrs
      endOfScenario
      pure s
    ScenarioResolution (Resolution 1) -> do
      story Flavor.resolution1
      record TheSunBanishedCnidathquaIntoTheDepths
      additionalRewards attrs
      allGainXp attrs
      endOfScenario
      pure s
    ScenarioResolution (Resolution 2) -> do
      story Flavor.resolution2
      record CnidathquaRetreatedToNurseItsWounds
      additionalRewards attrs
      allGainXp attrs
      endOfScenario
      pure s
    ChooseOneRewardByEachPlayer rewards@(_ : _) (currentInvestigatorId : rest) -> do
      chooseOneM currentInvestigatorId do
        labeled "Do not add a mask" $ push $ ChooseOneRewardByEachPlayer rewards rest
        for_ rewards \reward -> do
          cardLabeled reward do
            addCampaignCardToDeck currentInvestigatorId reward
            push $ ChooseOneRewardByEachPlayer (delete reward rewards) rest
      pure s
    _ -> CarnevaleOfHorrors <$> liftRunMessage msg attrs
