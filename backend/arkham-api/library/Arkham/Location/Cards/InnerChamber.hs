module Arkham.Location.Cards.InnerChamber (innerChamber) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers (getHasSupply)
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.RelicsOfThePast.Helpers

newtype InnerChamber = InnerChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innerChamber :: LocationCard InnerChamber
innerChamber =
  symbolLabel
    $ location InnerChamber Cards.innerChamber 4 (PerPlayer 2)
    & setConnectsTo (singleton RightOf)

instance HasAbilities InnerChamber where
  getAbilities (InnerChamber a) =
    extendRevealed1 a $ restricted a 1 Here $ actionAbilityWithCost ClueCostX

getClueCount :: Payment -> Int
getClueCount (CluePayment _ n) = n
getClueCount (Payments ps) = sum $ map getClueCount ps
getClueCount _ = 0

instance RunMessage InnerChamber where
  runMessage msg l@(InnerChamber attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getClueCount -> x) -> do
      push $ DrawCards iid $ targetCardDraw attrs ExplorationDeck x
      pure l
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      let cards = drewCards.cards
      let nonLocations = filter (not . (`cardMatch` CardWithType LocationType)) cards
      let
        drawCard card =
          push
            $ DrewCards iid
            $ CardDrew
              { cardDrewSource = toSource attrs
              , cardDrewDeck = Deck.ScenarioDeckByKey ExplorationDeck
              , cardDrewCards = [card]
              , cardDrewAction = False
              , cardDrewRules = mempty
              , cardDrewTarget = Nothing
              }
      if null nonLocations
        then focusCards cards do
          shuffleCardsIntoDeck ExplorationDeck cards
          continue_ iid
        else do
          hasChalk <- getHasSupply iid Chalk
          focusCards cards do
            chooseTargetM iid nonLocations \card -> do
              let rest = deleteFirst card cards
              let restNonLocations = deleteFirst card nonLocations
              if hasChalk && notNull restNonLocations
                then chooseOneM iid $ scenarioI18n do
                  labeled' "innerChamber.doNotDrawAnother" do
                    unfocusCards
                    shuffleCardsIntoDeck ExplorationDeck rest
                    drawCard card
                  targets restNonLocations \extra -> do
                    unfocusCards
                    shuffleCardsIntoDeck ExplorationDeck (deleteFirst extra rest)
                    drawCard card
                    drawCard extra
                else do
                  unfocusCards
                  shuffleCardsIntoDeck ExplorationDeck rest
                  drawCard card
      pure l
    _ -> InnerChamber <$> liftRunMessage msg attrs
