module Arkham.Scenario.Scenarios.BeforeTheBlackThrone (
  BeforeTheBlackThrone (..),
  beforeTheBlackThrone,
) where

import Arkham.Prelude hiding ((<|))

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.Direction
import Arkham.EncounterSet qualified as EncounterSet
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
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Scenarios.BeforeTheBlackThrone.Story
import Arkham.Token (addTokens)
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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

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

readInvestigatorDefeat :: HasGame m => m [Message]
readInvestigatorDefeat = do
  defeatedInvestigatorIds <- selectList DefeatedInvestigator
  if null defeatedInvestigatorIds
    then pure []
    else do
      players <- traverse getPlayer defeatedInvestigatorIds
      pure
        $ [story players investigatorDefeat]
        <> map DrivenInsane defeatedInvestigatorIds

instance RunMessage BeforeTheBlackThrone where
  runMessage msg s@(BeforeTheBlackThrone attrs) = case msg of
    PreScenarioSetup -> do
      players <- allPlayers
      pushAll [story players intro]
      pure s
    Setup -> do
      pathWindsBeforeYouCount <- getRecordCount ThePathWindsBeforeYou
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.piperOfAzathoth, Enemies.azathoth]
          [ EncounterSet.BeforeTheBlackThrone
          , EncounterSet.AgentsOfAzathoth
          , EncounterSet.InexorableFate
          , EncounterSet.AncientEvils
          , EncounterSet.DarkCult
          ]
      (cosmicIngress, placeCosmicIngress) <- placeLocationCard Locations.cosmicIngress

      cosmosCards' <-
        shuffleM
          =<< genCards
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

      hideousPalace <- genCard Locations.hideousPalace

      (firstCosmosCard, secondCosmosCard) <-
        shuffleM [topCosmosCard, hideousPalace] <&> \case
          [x, y] -> (x, y)
          _ -> error "did not have enough cards"

      lead <- getLead
      (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 6) lead

      (firstCosmos, placeFirstCosmos) <- placeLocation firstCosmosCard
      (secondCosmos, placeSecondCosmos) <- placeLocation secondCosmosCard

      let
        emptySpaceLocations =
          [ Pos 0 1
          , Pos 0 (-1)
          , Pos 1 1
          , Pos 1 0
          , Pos 1 (-1)
          , Pos 2 0
          ]
        emptySpaces = zip emptySpaceLocations cards

      let cosmos = initCosmos @Card @LocationId

      placeEmptySpaces <- concatForM emptySpaces $ \(pos, card) -> do
        (emptySpace', placeEmptySpace) <- placeLocationCard Locations.emptySpace
        pure [placeEmptySpace, PlaceCosmos lead emptySpace' (EmptySpace pos card)]

      azathoth <- genCard Enemies.azathoth
      createAzathoth <- toMessage <$> createEnemy azathoth Global

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetActDeck
          , SetAgendaDeck
          , placeCosmicIngress
          , PlaceCosmos lead cosmicIngress (CosmosLocation (Pos 0 0) cosmicIngress)
          , placeFirstCosmos
          , PlaceCosmos lead firstCosmos (CosmosLocation (Pos 2 1) firstCosmos)
          , placeSecondCosmos
          , PlaceCosmos lead secondCosmos (CosmosLocation (Pos 2 (-1)) secondCosmos)
          , MoveAllTo (toSource attrs) cosmicIngress
          , createAzathoth
          ]
        <> map (ObtainCard . toCard) cards
        <> placeEmptySpaces

      setAsideCards <-
        genCards [Locations.courtOfTheGreatOldOnes, Locations.theBlackThrone, Enemies.piperOfAzathoth]

      agendas <- genCards [Agendas.wheelOfFortuneX, Agendas.itAwaits, Agendas.theFinalCountdown]
      acts <- genCards [Acts.theCosmosBeckons, Acts.inAzathothsDomain, Acts.whatMustBeDone]

      BeforeTheBlackThrone
        <$> runMessage
          msg
          ( attrs
              & (decksL . at CosmosDeck ?~ cosmosCards)
              & locationLayoutL
              .~ cosmosToGrid cosmos
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
              & (metaL .~ toJSON cosmos)
              & (usesGridL .~ True)
              & (setAsideCardsL .~ setAsideCards)
              & (tokensL %~ addTokens Token.Resource pathWindsBeforeYouCount)
          )
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
        Cultist -> push $ findAndDrawEncounterCard iid (CardWithTrait Trait.Cultist)
        Tablet -> do
          azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
          push $ EnemyAttack $ enemyAttack azathoth (ChaosTokenEffectSource Tablet) iid
        _ -> pure ()
      pure s
    ResolveChaosToken _ ElderThing _ -> do
      v <- getSkillTestModifiedSkillValue
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      pushWhen (v == 0) $ PlaceDoom (ChaosTokenEffectSource ElderThing) (toTarget azathoth) 1
      pure s
    ScenarioResolution n -> do
      investigators <- getInvestigators
      players <- allPlayers
      case n of
        NoResolution -> push R1
        Resolution x | x == 1 || x == 11 -> do
          msgs <- if x == 1 then readInvestigatorDefeat else pure []
          pushAll
            $ msgs
            <> [story players resolution1, Record AzathothDevouredTheUniverse]
            <> map (InvestigatorKilled (toSource attrs)) investigators
            <> [GameOver]
        Resolution 2 -> do
          lead <- getLead
          msgs <- readInvestigatorDefeat
          gainXp <- toGainXp (toSource attrs) (getXpWithBonus 5)
          pushAll
            $ msgs
            <> [ story players resolution2
               , Record TheLeadInvestigatorHasJoinedThePipersOfAzathoth
               , Record AzathothSlumbersForNow
               , DrivenInsane lead
               ]
            <> gainXp
            <> [SufferTrauma investigator 0 2 | investigator <- investigators]
            <> [EndOfGame Nothing]
        Resolution 3 -> do
          msgs <- readInvestigatorDefeat
          gainXp <- toGainXp (toSource attrs) (getXpWithBonus 5)
          pushAll
            $ msgs
            <> [ story players resolution3
               , Record AzathothSlumbersForNow
               ]
            <> gainXp
            <> [SufferTrauma investigator 2 0 | investigator <- investigators]
            <> [EndOfGame Nothing]
        Resolution 4 -> do
          msgs <- readInvestigatorDefeat
          lead <- getLeadPlayer
          pushAll
            $ msgs
            <> [ storyWithChooseOne
                  lead
                  players
                  resolution4
                  [Label "It must be done." [R5], Label "I refuse" [ScenarioResolution (Resolution 11)]]
               ]
        Resolution 5 -> do
          gainXp <- toGainXp (toSource attrs) (getXpWithBonus 10)
          pushAll
            $ [ story players resolution5
              , Record AzathothSlumbersForNow
              , Record TheInvestigatorsSignedTheBlackBookOfAzathoth
              ]
            <> gainXp
            <> [SufferTrauma investigator 2 2 | investigator <- investigators]
            <> [EndOfGame Nothing]
        _ -> error "unknown resolution"
      pure s
    _ -> BeforeTheBlackThrone <$> runMessage msg attrs
