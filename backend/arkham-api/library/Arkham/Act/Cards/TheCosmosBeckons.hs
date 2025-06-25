module Arkham.Act.Cards.TheCosmosBeckons (theCosmosBeckons) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import Arkham.Enemy.Types qualified as Field
import Arkham.Helpers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types qualified as Field
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Data.List qualified as List
import Data.List.Extra qualified as List

newtype TheCosmosBeckons = TheCosmosBeckons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCosmosBeckons :: ActCard TheCosmosBeckons
theCosmosBeckons =
  act
    (1, A)
    TheCosmosBeckons
    Cards.theCosmosBeckons
    (Just $ GroupClueCost (PerPlayer 1) "Hideous Palace")

instance HasAbilities TheCosmosBeckons where
  getAbilities (TheCosmosBeckons attrs) =
    extend attrs [mkAbility attrs 1 $ actionAbilityWithCost ClueCostX]

getClueCount :: Payment -> Int
getClueCount (CluePayment _ n) = n
getClueCount (Payments ps) = sum $ map getClueCount ps
getClueCount _ = 0

instance RunMessage TheCosmosBeckons where
  runMessage msg a@(TheCosmosBeckons attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getClueCount -> x) -> do
      push $ DrawCards iid $ targetCardDraw attrs CosmosDeck x
      pure a
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      let cards = drewCards.cards
      cardsWithMsgs <- traverse (traverseToSnd placeLocation) cards
      player <- getPlayer iid
      pushAll
        [ FocusCards $ map flipCard cards
        , chooseOrRunOne
            player
            [ targetLabel
                (toCardId card)
                [ ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) (List.delete card cards)
                , placement
                , RevealLocation (Just iid) lid
                , RunCosmos iid lid [Move $ move (toAbilitySource attrs 1) iid lid]
                , UnfocusCards
                ]
            | (card, (lid, placement)) <- cardsWithMsgs
            ]
        ]
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      cosmicIngress <- getJustLocationByName "Cosmic Ingress"
      emptySpace <- select $ IncludeEmptySpace $ locationIs Locations.emptySpace
      emptySpaceCards <- getEmptySpaceCards
      cosmosLocations <-
        select
          $ NotLocation
          $ LocationMatchAny [LocationWithTitle "Hideous Palace", LocationWithTitle "Cosmic Ingress"]
      enemies <- select $ EnemyAt $ NotLocation $ LocationWithTitle "Hideous Palace"
      enemyCards <- traverse (field Field.EnemyCard) enemies

      cosmosCards <- traverse (field Field.LocationCard) cosmosLocations

      let cosmos' = initCosmos @Card @LocationId
          cardsWithOwners = List.groupOnKey toCardOwner emptySpaceCards

      pushAll
        $ RemoveLocation cosmicIngress
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
      (cards, cosmosDeck) <- splitAt 2 <$> getScenarioDeck CosmosDeck
      courtOfTheGreatOldOnes <- getSetAsideCard Locations.courtOfTheGreatOldOnes
      hideousPalace <- getJustLocationByName "Hideous Palace"
      lead <- getLead
      (firstCosmosCard, secondCosmosCard, thirdCosmosCard) <-
        shuffleM (courtOfTheGreatOldOnes : cards) <&> \case
          [x, y, z] -> (x, y, z)
          _ -> error "impossible"

      (firstCosmos, placeFirstCosmos) <- placeLocation firstCosmosCard
      (secondCosmos, placeSecondCosmos) <- placeLocation secondCosmosCard
      (thirdCosmos, placeThirdCosmos) <- placeLocation thirdCosmosCard

      (map toCard -> playerCards, _) <- fieldMap InvestigatorDeck (draw 7) lead

      let
        emptySpaceLocations =
          [ Pos 0 1
          , Pos 1 1
          , Pos 2 1
          , Pos 1 0
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
          , PlaceCosmos lead hideousPalace (CosmosLocation (Pos 0 0) hideousPalace)
          , placeFirstCosmos
          , PlaceCosmos lead firstCosmos (CosmosLocation (Pos 1 2) firstCosmos)
          , placeSecondCosmos
          , PlaceCosmos lead secondCosmos (CosmosLocation (Pos 1 (-2)) secondCosmos)
          , placeThirdCosmos
          , PlaceCosmos lead thirdCosmos (CosmosLocation (Pos 2 0) thirdCosmos)
          ]
        <> map (ObtainCard . toCardId) playerCards
        <> placeEmptySpaces
      pure a
    _ -> TheCosmosBeckons <$> runMessage msg attrs
