module Arkham.Act.Cards.InAzathothsDomain (
  InAzathothsDomain (..),
  inAzathothsDomain,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types qualified as Field
import Arkham.Helpers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Data.List qualified as List
import Data.List.Extra qualified as List

newtype InAzathothsDomain = InAzathothsDomain ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inAzathothsDomain :: ActCard InAzathothsDomain
inAzathothsDomain =
  act
    (2, A)
    InAzathothsDomain
    Cards.inAzathothsDomain
    (Just $ GroupClueCost (PerPlayer 2) "Court of the Great Old Ones")

instance HasAbilities InAzathothsDomain where
  getAbilities (InAzathothsDomain attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
            $ ActionAbility []
            $ ActionCost 1
            <> ClueCostX
        ]

getClueCount :: Payment -> Int
getClueCount (CluePayment _ n) = n
getClueCount (Payments ps) = sum $ map getClueCount ps
getClueCount _ = 0

instance RunMessage InAzathothsDomain where
  runMessage msg a@(InAzathothsDomain attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getClueCount -> x) -> do
      push $ DrawFromScenarioDeck iid CosmosDeck (toTarget attrs) x
      pure a
    DrewFromScenarioDeck iid _ (isTarget attrs -> True) cards -> do
      cardsWithMsgs <- traverse (traverseToSnd placeLocation) cards
      player <- getPlayer iid
      pushAll
        [ FocusCards $ map flipCard cards
        , chooseOrRunOne
            player
            [ targetLabel
              (toCardId card)
              [ UnfocusCards
              , ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) (List.delete card cards)
              , placement
              , Msg.RevealLocation (Just iid) lid
              , RunCosmos iid lid [Move $ move (toAbilitySource attrs 1) iid lid]
              ]
            | (card, (lid, placement)) <- cardsWithMsgs
            ]
        ]
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      hideousPalace <- getJustLocationByName "Hideous Palace"
      emptySpace <- select $ IncludeEmptySpace $ locationIs Locations.emptySpace
      emptySpaceCards <- getEmptySpaceCards
      cosmosLocations <-
        select
          $ NotLocation
          $ LocationMatchAny
            [LocationWithTitle "Court of the Great Old Ones", LocationWithTitle "Hideous Palace"]
      enemies <- select $ EnemyAt $ NotLocation $ LocationWithTitle "Court of the Great Old Ones"
      enemyCards <- traverse (field Field.EnemyCard) enemies

      cosmosCards <- traverse (field Field.LocationCard) cosmosLocations

      let cosmos' = initCosmos @Card @LocationId
          cardsWithOwners = List.groupOnKey toCardOwner emptySpaceCards

      pushAll
        $ RemoveLocation hideousPalace
        : map RemoveLocation (cosmosLocations <> emptySpace)
          <> [ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) cosmosCards]
          <> [ShuffleCardsIntoTopOfDeck Deck.EncounterDeck 5 enemyCards]
          <> [ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) cards | (Just iid, cards) <- cardsWithOwners]
          <> [ SetScenarioMeta (toJSON cosmos')
             , NextAdvanceActStep (toId a) 1
             , AllDrawEncounterCard
             , advanceActDeck attrs
             ]
      pure a
    NextAdvanceActStep aid _ | aid == toId attrs -> do
      (cards, cosmosDeck) <- splitAt 3 <$> getScenarioDeck CosmosDeck
      theBlackThrone <- getSetAsideCard Locations.theBlackThrone
      courtOfTheGreatOldOnes <- getJustLocationByName "Court of the Great Old Ones"
      lead <- getLead
      (firstCosmosCard, secondCosmosCard, thirdCosmosCard, fourthCosmosCard) <-
        shuffleM (theBlackThrone : cards) <&> \case
          [x, y, z, b] -> (x, y, z, b)
          _ -> error "impossible"

      (firstCosmos, placeFirstCosmos) <- placeLocation firstCosmosCard
      (secondCosmos, placeSecondCosmos) <- placeLocation secondCosmosCard
      (thirdCosmos, placeThirdCosmos) <- placeLocation thirdCosmosCard
      (fourthCosmos, placeFourthCosmos) <- placeLocation fourthCosmosCard

      (map toCard -> playerCards, _) <- fieldMap InvestigatorDeck (draw 8) lead

      let
        emptySpaceLocations =
          [ Pos 0 1
          , Pos 1 1
          , Pos 2 1
          , Pos 1 0
          , Pos 2 0
          , Pos 0 (-1)
          , Pos 1 (-1)
          , Pos 2 (-1)
          ]
        emptySpaces = zip emptySpaceLocations playerCards

      placeEmptySpaces <- concatForM emptySpaces $ \(pos, card) -> do
        (emptySpace', placeEmptySpace) <- placeLocationCard Locations.emptySpace
        pure [placeEmptySpace, PlaceCosmos lead emptySpace' (EmptySpace pos card)]

      pushAll
        $ [ SetScenarioDeck CosmosDeck cosmosDeck
          , PlaceCosmos lead courtOfTheGreatOldOnes (CosmosLocation (Pos 0 0) courtOfTheGreatOldOnes)
          , placeFirstCosmos
          , PlaceCosmos lead firstCosmos (CosmosLocation (Pos 1 2) firstCosmos)
          , placeSecondCosmos
          , PlaceCosmos lead secondCosmos (CosmosLocation (Pos 1 (-2)) secondCosmos)
          , placeThirdCosmos
          , PlaceCosmos lead thirdCosmos (CosmosLocation (Pos 3 1) thirdCosmos)
          , placeFourthCosmos
          , PlaceCosmos lead fourthCosmos (CosmosLocation (Pos 3 (-1)) fourthCosmos)
          ]
        <> map (ObtainCard . toCard) playerCards
        <> placeEmptySpaces
      pure a
    _ -> InAzathothsDomain <$> runMessage msg attrs
