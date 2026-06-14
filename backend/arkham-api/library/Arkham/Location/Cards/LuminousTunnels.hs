module Arkham.Location.Cards.LuminousTunnels (luminousTunnels) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LuminousTunnels = LuminousTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousTunnels :: LocationCard LuminousTunnels
luminousTunnels = location LuminousTunnels Cards.luminousTunnels 2 (Static 2)

instance HasAbilities LuminousTunnels where
  getAbilities (LuminousTunnels a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ actionAbilityWithCost (DamageCost (toSource a) YouTarget 1)

instance RunMessage LuminousTunnels where
  runMessage msg l@(LuminousTunnels attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Shuffle the set-aside Pilgrims set into the encounter deck along with the
      -- encounter discard pile.
      shuffleEncounterDiscardBackIn
      shuffleSetAsideEncounterSet Set.Pilgrims
      -- "Find Spawning Grounds and Corrupted Vault and shuffle them into the
      -- bottom 10 cards of the encounter deck." There is no message for shuffling
      -- into the bottom N yet, so place them on the bottom of the encounter deck.
      -- TODO: shuffle into the bottom 10 specifically once a message exists.
      spawningGrounds <- getSetAsideCard Cards.spawningGrounds
      corruptedVault <- getSetAsideCard Cards.corruptedVault
      putCardOnBottomOfDeck iid Deck.EncounterDeck spawningGrounds
      putCardOnBottomOfDeck iid Deck.EncounterDeck corruptedVault
      LuminousTunnels <$> liftRunMessage msg attrs
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      apiaryEntrance <- getJustLocationByName "Apiary Entrance"
      farthest <- select $ FarthestLocationFromLocation apiaryEntrance Anywhere
      for_ farthest $ placeCluesUpToClueValue (attrs.ability 1)
      pure l
    _ -> LuminousTunnels <$> liftRunMessage msg attrs
