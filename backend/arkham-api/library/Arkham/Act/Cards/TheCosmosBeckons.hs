module Arkham.Act.Cards.TheCosmosBeckons (theCosmosBeckons) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
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
import Arkham.Message.Lifted.Choose
import Arkham.Movement
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
  runMessage msg a@(TheCosmosBeckons attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getClueCount -> x) -> do
      push $ DrawCards iid $ targetCardDraw attrs CosmosDeck x
      pure a
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      let cards = drewCards.cards
      focusCards (map flipCard cards) do
        chooseOrRunOneM iid do
          targets cards \card -> do
            shuffleCardsIntoDeck CosmosDeck (List.delete card cards)
            lid <- placeLocation card
            revealBy iid lid
            movemsg <- move (attrs.ability 1) iid lid
            push $ RunCosmos iid lid [Move movemsg]
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
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

      for_ (cosmicIngress : cosmosLocations <> emptySpace) removeLocation
      shuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) cosmosCards
      shuffleCardsIntoTopOfDeck Deck.EncounterDeck 5 enemyCards
      for_ cardsWithOwners \(mowner, cards) -> for_ mowner \iid -> do
        shuffleCardsIntoDeck iid cards
      push $ SetScenarioMeta (toJSON cosmos')
      doStep 1 msg
      allDrawEncounterCard
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      (cards, cosmosDeck) <- splitAt 2 <$> getScenarioDeck CosmosDeck
      courtOfTheGreatOldOnes <- getSetAsideCard Locations.courtOfTheGreatOldOnes
      hideousPalace <- getJustLocationByName "Hideous Palace"
      lead <- getLead
      (firstCosmosCard, secondCosmosCard, thirdCosmosCard) <-
        shuffleM (courtOfTheGreatOldOnes : cards) <&> \case
          [x, y, z] -> (x, y, z)
          _ -> error "impossible"

      (map toCard -> playerCards, _) <- fieldMap InvestigatorDeck (draw 7) lead

      setScenarioDeck CosmosDeck cosmosDeck
      push $ PlaceCosmos lead hideousPalace (CosmosLocation (Pos 0 0) hideousPalace)

      firstCosmos <- placeLocation firstCosmosCard
      push $ PlaceCosmos lead firstCosmos (CosmosLocation (Pos 1 2) firstCosmos)
      secondCosmos <- placeLocation secondCosmosCard
      push $ PlaceCosmos lead secondCosmos (CosmosLocation (Pos 1 (-2)) secondCosmos)
      thirdCosmos <- placeLocation thirdCosmosCard
      push $ PlaceCosmos lead thirdCosmos (CosmosLocation (Pos 2 0) thirdCosmos)

      for_ playerCards obtainCard

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

      for_ emptySpaces \(pos, card) -> do
        emptySpace' <- placeLocationCard Locations.emptySpace
        push $ PlaceCosmos lead emptySpace' (EmptySpace pos card)
      pure a
    _ -> TheCosmosBeckons <$> liftRunMessage msg attrs
