module Arkham.Scenario.Scenarios.BeforeTheBlackThrone (BeforeTheBlackThrone (..), beforeTheBlackThrone) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Label (mkLabel)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (defeated)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (locationLayoutL, metaL)
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Scenarios.BeforeTheBlackThrone.Story
import Arkham.Token qualified as Token
import Arkham.Trait qualified as Trait
import Data.Aeson (Result (..))

-- * Cosmos

{- $cosmos
The cosmos is an internal data structure that represents a grid which can
grow in any direction. Additionally, we need to track if there is empty
space or a card at a specific position. Finally, cards can slide around.
Because this logic needs to be known by locations we store in the
`scenarioMeta` field. That way it can be passed around and set accordingly.
-}

newtype BeforeTheBlackThrone = BeforeTheBlackThrone ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beforeTheBlackThrone :: Difficulty -> BeforeTheBlackThrone
beforeTheBlackThrone difficulty = scenario BeforeTheBlackThrone "05325" "Before the Black Throne" difficulty []

instance HasChaosTokenValue BeforeTheBlackThrone where
  getChaosTokenValue iid tokenFace (BeforeTheBlackThrone attrs) = case tokenFace of
    Skull -> do
      x <- selectJustField EnemyDoom (IncludeOmnipotent $ enemyIs Enemies.azathoth)
      pure $ toChaosTokenValue attrs Skull (max 2 (x `div` 2)) (max 2 x)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 6
    otherFace -> getChaosTokenValue iid otherFace attrs

readInvestigatorDefeat :: ReverseQueue m => m ()
readInvestigatorDefeat = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    storyOnly defeated investigatorDefeat
    for_ defeated drivenInsane

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
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

instance RunMessage BeforeTheBlackThrone where
  runMessage msg s@(BeforeTheBlackThrone attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    PreScenarioSetup -> do
      story intro
      pure s
    Setup -> runScenarioSetup BeforeTheBlackThrone attrs do
      pathWindsBeforeYouCount <- getRecordCount ThePathWindsBeforeYou
      gather Set.BeforeTheBlackThrone
      gather Set.AgentsOfAzathoth
      gather Set.InexorableFate
      gather Set.AncientEvils
      gather Set.DarkCult

      cosmicIngress <- place Locations.cosmicIngress
      startAt cosmicIngress

      cosmosCards' <-
        shuffle
          [ Locations.infinityOfDarkness
          , Locations.infinityOfDarkness
          , Locations.infinityOfDarkness
          , Locations.cosmicGate
          , Locations.pathwayIntoVoid
          , Locations.pathwayIntoVoid
          , Locations.dancersMist
          , Locations.dancersMist
          , Locations.dancersMist
          , Locations.flightIntoOblivion
          , Locations.flightIntoOblivion
          , Locations.flightIntoOblivion
          ]

      let
        (topCosmosCard, cosmosCards) =
          case cosmosCards' of
            (x : xs) -> (x, xs)
            _ -> error "did not have enough cards"

      (firstCosmosCard, secondCosmosCard) <-
        shuffleM [topCosmosCard, Locations.hideousPalace] <&> \case
          [x, y] -> (x, y)
          _ -> error "did not have enough cards"

      firstCosmos <- place firstCosmosCard
      secondCosmos <- place secondCosmosCard

      lead <- getLead
      (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 6) lead
      let
        emptySpaceLocations = [Pos 0 1, Pos 0 (-1), Pos 1 1, Pos 1 0, Pos 1 (-1), Pos 2 0]
        emptySpaces = zip emptySpaceLocations cards

      let cosmos = initCosmos @Card @LocationId

      placeEnemy Enemies.azathoth Global

      pushAll
        $ [ PlaceCosmos lead cosmicIngress (CosmosLocation (Pos 0 0) cosmicIngress)
          , PlaceCosmos lead firstCosmos (CosmosLocation (Pos 2 1) firstCosmos)
          , PlaceCosmos lead secondCosmos (CosmosLocation (Pos 2 (-1)) secondCosmos)
          ]
        <> map (ObtainCard . toCardId) cards

      for_ emptySpaces $ \(pos, card) -> do
        emptySpace' <- placeLocationCard Locations.emptySpace
        push $ PlaceCosmos lead emptySpace' (EmptySpace pos card)

      setAside [Locations.courtOfTheGreatOldOnes, Locations.theBlackThrone, Enemies.piperOfAzathoth]
      setAgendaDeck [Agendas.wheelOfFortuneX, Agendas.itAwaits, Agendas.theFinalCountdown]
      setActDeck [Acts.theCosmosBeckons, Acts.inAzathothsDomain, Acts.whatMustBeDone]
      setLayout $ cosmosToGrid cosmos
      addExtraDeck CosmosDeck cosmosCards
      setMeta cosmos
      setUsesGrid
      placeTokensOnScenarioReference Token.Resource pathWindsBeforeYouCount
    SetScenarioMeta meta -> do
      case fromJSON @(Cosmos Card LocationId) meta of
        Error err -> error err
        Success cosmos -> pure $ BeforeTheBlackThrone $ attrs & metaL .~ meta & locationLayoutL .~ cosmosToGrid cosmos
    PlaceCosmos _ lid cloc -> do
      cosmos' <- getCosmos
      let
        pos = cosmosLocationToPosition cloc
        current = viewCosmos pos cosmos'
        cosmos'' = insertCosmos cloc cosmos'
      mTopLocation <-
        selectOne
          $ IncludeEmptySpace
          $ LocationWithLabel (mkLabel $ cosmicLabel $ updatePosition pos GridUp)
      mBottomLocation <-
        selectOne
          $ IncludeEmptySpace
          $ LocationWithLabel (mkLabel $ cosmicLabel $ updatePosition pos GridDown)
      mLeftLocation <-
        selectOne
          $ IncludeEmptySpace
          $ LocationWithLabel (mkLabel $ cosmicLabel $ updatePosition pos GridLeft)
      mRightLocation <-
        selectOne
          $ IncludeEmptySpace
          $ LocationWithLabel (mkLabel $ cosmicLabel $ updatePosition pos GridRight)
      currentMsgs <- case current of
        Just (EmptySpace _ c) -> case toCardOwner c of
          Nothing -> error "Unhandled"
          Just iid -> do
            emptySpace <- selectJust $ IncludeEmptySpace $ LocationWithLabel (mkLabel $ cosmicLabel pos)
            pure [RemoveFromGame (toTarget emptySpace), ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [c]]
        _ -> pure []
      pushAll
        $ currentMsgs
        <> [ LocationMoved lid
           , SetLocationLabel lid (cosmicLabel pos)
           , SetScenarioMeta (toJSON cosmos'')
           ]
        <> [PlacedLocationDirection lid Below topLocation | topLocation <- maybeToList mTopLocation]
        <> [PlacedLocationDirection lid Above bottomLocation | bottomLocation <- maybeToList mBottomLocation]
        <> [PlacedLocationDirection lid LeftOf rightLocation | rightLocation <- maybeToList mRightLocation]
        <> [PlacedLocationDirection lid RightOf leftLocation | leftLocation <- maybeToList mLeftLocation]

      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist -> findAndDrawEncounterCard iid (CardWithTrait Trait.Cultist)
        Tablet -> do
          azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
          initiateEnemyAttack azathoth Tablet iid
        _ -> pure ()
      pure s
    ResolveChaosToken _ ElderThing _ -> do
      v <- getSkillTestModifiedSkillValue
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      when (v == 0) $ placeDoom ElderThing azathoth 1
      pure s
    ScenarioResolution n -> do
      case n of
        NoResolution -> push R1
        Resolution x | x == 1 || x == 11 -> do
          when (x == 1) readInvestigatorDefeat
          story resolution1
          record AzathothDevouredTheUniverse
          eachInvestigator (kill attrs)
          gameOver
        Resolution 2 -> do
          readInvestigatorDefeat
          story resolution2
          record TheLeadInvestigatorHasJoinedThePipersOfAzathoth
          record AzathothSlumbersForNow
          drivenInsane =<< getLead
          allGainXpWithBonus attrs $ toBonus "resolution2" 5
          eachInvestigator (`sufferMentalTrauma` 2)
          endOfScenario
        Resolution 3 -> do
          readInvestigatorDefeat
          story resolution3
          record AzathothSlumbersForNow
          allGainXpWithBonus attrs $ toBonus "resolution3" 5
          eachInvestigator (`sufferPhysicalTrauma` 2)
          endOfScenario
        Resolution 4 -> do
          readInvestigatorDefeat
          storyWithChooseOneM resolution4 do
            labeled "It must be done." $ push R5
            labeled "I refuse" $ push $ ScenarioResolution (Resolution 11) -- actually 1
        Resolution 5 -> do
          readInvestigatorDefeat
          story resolution5
          record AzathothSlumbersForNow
          record TheInvestigatorsSignedTheBlackBookOfAzathoth
          allGainXpWithBonus attrs $ toBonus "resolution5" 10
          eachInvestigator \iid -> sufferTrauma iid 2 2
          endOfScenario
        _ -> error "unknown resolution"
      pure s
    _ -> BeforeTheBlackThrone <$> liftRunMessage msg attrs
