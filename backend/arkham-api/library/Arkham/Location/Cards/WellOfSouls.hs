module Arkham.Location.Cards.WellOfSouls (wellOfSouls) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype WellOfSouls = WellOfSouls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellOfSouls :: LocationCard WellOfSouls
wellOfSouls =
  locationWith WellOfSouls Cards.wellOfSouls 4 (PerPlayer 1)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities WellOfSouls where
  getAbilities (WellOfSouls attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 Here $ forced $ TurnEnds #after You
      , restricted
          attrs
          2
          (oneOf [notExists $ LocationInDirection dir (be attrs) | dir <- [Above, Below, RightOf]])
          $ forced
          $ RevealLocation #when Anyone (be attrs)
      ]

instance RunMessage WellOfSouls where
  runMessage msg l@(WellOfSouls attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasCardsInHand <- selectAny $ inHandOf NotForPlay iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDirectHorror" $ directHorror iid (attrs.ability 1) 1
        when hasCardsInHand do
          countVar 2 $ labeled' "discardRandomCardsFromHand" $ randomDiscardN iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck 1
      pure l
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      case drewCards.cards of
        [card] -> do
          aboveEmpty <- directionEmpty attrs Above
          belowEmpty <- directionEmpty attrs Below
          rightEmpty <- directionEmpty attrs RightOf
          chooseOrRunOneM iid $ scenarioI18n do
            when aboveEmpty $ labeled' "above" $ placeAtDirection_ Above attrs card
            when belowEmpty $ labeled' "below" $ placeAtDirection_ Below attrs card
            when rightEmpty $ labeled' "right" $ placeAtDirection_ RightOf attrs card
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> WellOfSouls <$> liftRunMessage msg attrs
