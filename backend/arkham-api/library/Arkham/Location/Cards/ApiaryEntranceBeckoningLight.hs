module Arkham.Location.Cards.ApiaryEntranceBeckoningLight (apiaryEntranceBeckoningLight) where

import Arkham.Ability
import Arkham.Card (filterLocations, toCard)
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario (getEncounterDiscard)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
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
      [ restricted a 1 Here $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
      , groupLimit PerRound
          $ restricted a 2 (exists $ FarthestLocationFromLocation a.id Anywhere)
          $ FastAbility Free
      , scenarioI18n $ withI18nTooltip "apiaryEntranceBeckoningLight.resign" $ locationResignAction a
      ]

instance RunMessage ApiaryEntranceBeckoningLight where
  runMessage msg l@(ApiaryEntranceBeckoningLight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardPile <- getEncounterDiscard RegularEncounterDeck
      let locations = filterLocations (map toCard discardPile)
      -- "Shuffle each location in the encounter discard pile into the bottom 10
      -- cards of the encounter deck." There is no message for shuffling into the
      -- bottom N specifically yet, so place them on the bottom of the encounter deck.
      -- TODO: shuffle into the bottom 10 specifically once a message exists.
      for_ locations $ putCardOnBottomOfDeck iid Deck.EncounterDeck
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      farthest <- select $ FarthestLocationFromLocation attrs.id Anywhere
      for_ farthest $ placeCluesUpToClueValue (attrs.ability 2)
      pure l
    _ -> ApiaryEntranceBeckoningLight <$> liftRunMessage msg attrs
