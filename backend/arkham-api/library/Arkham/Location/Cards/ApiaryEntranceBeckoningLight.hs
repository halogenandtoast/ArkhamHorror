module Arkham.Location.Cards.ApiaryEntranceBeckoningLight (apiaryEntranceBeckoningLight) where

import Arkham.Ability
import Arkham.Card (filterLocations, toCard)
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario (getEncounterDiscard)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (..))
import Arkham.Scenarios.TheApiary.Helpers

newtype ApiaryEntranceBeckoningLight = ApiaryEntranceBeckoningLight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apiaryEntranceBeckoningLight :: LocationCard ApiaryEntranceBeckoningLight
apiaryEntranceBeckoningLight = location ApiaryEntranceBeckoningLight Cards.apiaryEntranceBeckoningLight 1 (Static 1)

instance HasAbilities ApiaryEntranceBeckoningLight where
  getAbilities (ApiaryEntranceBeckoningLight a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> exists (InEncounterDiscard <> basic #location))
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
      , groupLimit PerRound
          $ restricted a 2 (exists $ RevealedLocation <> LocationNotAtClueLimit)
          $ FastAbility Free
      , scenarioI18n $ withI18nTooltip "apiaryEntranceBeckoningLight.resign" $ locationResignAction a
      ]

instance RunMessage ApiaryEntranceBeckoningLight where
  runMessage msg l@(ApiaryEntranceBeckoningLight attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      discardPile <- getEncounterDiscard RegularEncounterDeck
      let locations = filterLocations (map toCard discardPile)
      shuffleCardsIntoBottomOfDeck Deck.EncounterDeck 10 locations
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      farthest <- select $ FarthestLocationFromLocation attrs.id Anywhere
      chooseTargetM iid farthest $ placeCluesUpToClueValue (attrs.ability 2)
      pure l
    _ -> ApiaryEntranceBeckoningLight <$> liftRunMessage msg attrs
