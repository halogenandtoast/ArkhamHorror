module Arkham.Location.Cards.LuminousTunnels (luminousTunnels) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype LuminousTunnels = LuminousTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousTunnels :: LocationCard LuminousTunnels
luminousTunnels = location LuminousTunnels Cards.luminousTunnels 2 (Static 2)

instance HasAbilities LuminousTunnels where
  getAbilities (LuminousTunnels a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> exists (FarthestLocationFromLocationMatching "Apiary Entrance" Anywhere <> LocationNotAtClueLimit)
        )
      $ actionAbilityWithCost (DamageCost (toSource a) YouTarget 1)

instance RunMessage LuminousTunnels where
  runMessage msg l@(LuminousTunnels attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      -- Shuffle the set-aside Pilgrims set into the encounter deck along with the
      -- encounter discard pile.
      shuffleEncounterDiscardBackIn
      shuffleSetAsideEncounterSet Set.Pilgrims
      spawningGrounds <- getSetAsideCard Cards.spawningGrounds
      corruptedVault <- getSetAsideCard Cards.corruptedVault
      shuffleCardsIntoBottomOfDeck Deck.EncounterDeck 10 [spawningGrounds, corruptedVault]
      LuminousTunnels <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      farthest <-
        select $ FarthestLocationFromLocationMatching "Apiary Entrance" Anywhere <> LocationNotAtClueLimit
      chooseTargetM iid farthest $ placeCluesUpToClueValue (attrs.ability 1)
      pure l
    _ -> LuminousTunnels <$> liftRunMessage msg attrs
