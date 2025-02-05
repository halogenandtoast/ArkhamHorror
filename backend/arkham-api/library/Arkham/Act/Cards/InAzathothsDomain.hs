module Arkham.Act.Cards.InAzathothsDomain (inAzathothsDomain) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types qualified as Field
import Arkham.Helpers
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
    extend attrs [mkAbility attrs 1 $ actionAbilityWithCost ClueCostX]

instance RunMessage InAzathothsDomain where
  runMessage msg a@(InAzathothsDomain attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePayment -> x) -> do
      drawCardsEdit iid (attrs.ability 1) x (setTarget attrs)
      pure a
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      focusCards (map flipCard drewCards.cards) do
        chooseOrRunOneM iid do
          targets drewCards.cards \card -> do
            unfocusCards
            shuffleCardsIntoDeck CosmosDeck $ List.delete card drewCards.cards
            lid <- placeLocation card
            revealBy iid lid
            push $ RunCosmos iid lid [Move $ move (attrs.ability 1) iid lid]
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      hideousPalace <- getJustLocationByName "Hideous Palace"
      emptySpace <- select $ IncludeEmptySpace $ locationIs Locations.emptySpace
      emptySpaceCards <- getEmptySpaceCards
      cosmosLocations <- select $ NotLocation $ oneOf ["Court of the Great Old Ones", "Hideous Palace"]
      enemies <- select $ EnemyAt $ IncludeEmptySpace $ NotLocation "Court of the Great Old Ones"
      enemyCards <- traverse (field Field.EnemyCard) enemies
      cosmosCards <- traverse (field Field.LocationCard) cosmosLocations

      let cosmos' = initCosmos @Card @LocationId
          cardsWithOwners = List.groupOnKey toCardOwner emptySpaceCards

      removeLocation hideousPalace
      for_ (cosmosLocations <> emptySpace) removeLocation
      shuffleCardsIntoDeck CosmosDeck cosmosCards
      for_ enemyCards obtainCard
      shuffleCardsIntoTopOfDeck Deck.EncounterDeck 5 enemyCards
      for_ cardsWithOwners \(mOwner, cards) -> for_ mOwner \owner -> do
        shuffleCardsIntoDeck owner cards
      pushAll
        [ SetScenarioMeta (toJSON cosmos')
        , NextAdvanceActStep (toId a) 1
        , AllDrawEncounterCard
        ]
      advanceActDeck attrs
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

      (map toCard -> playerCards, _) <- fieldMap InvestigatorDeck (draw 8) lead
      for_ playerCards obtainCard

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

      setScenarioDeck CosmosDeck cosmosDeck

      push $ PlaceCosmos lead courtOfTheGreatOldOnes (CosmosLocation (Pos 0 0) courtOfTheGreatOldOnes)

      firstCosmos <- placeLocation firstCosmosCard
      push $ PlaceCosmos lead firstCosmos (CosmosLocation (Pos 1 2) firstCosmos)

      secondCosmos <- placeLocation secondCosmosCard
      push $ PlaceCosmos lead secondCosmos (CosmosLocation (Pos 1 (-2)) secondCosmos)

      thirdCosmos <- placeLocation thirdCosmosCard
      push $ PlaceCosmos lead thirdCosmos (CosmosLocation (Pos 3 1) thirdCosmos)

      fourthCosmos <- placeLocation fourthCosmosCard
      push $ PlaceCosmos lead fourthCosmos (CosmosLocation (Pos 3 (-1)) fourthCosmos)

      for_ emptySpaces $ \(pos, card) -> do
        emptySpace' <- placeLocationCard Locations.emptySpace
        push $ PlaceCosmos lead emptySpace' (EmptySpace pos card)

      pure a
    _ -> InAzathothsDomain <$> liftRunMessage msg attrs
