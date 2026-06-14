module Arkham.Location.Cards.GraspingCorridor (graspingCorridor) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GraspingCorridor = GraspingCorridor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graspingCorridor :: LocationCard GraspingCorridor
graspingCorridor = location GraspingCorridor Cards.graspingCorridor 2 (Static 2)

instance HasAbilities GraspingCorridor where
  getAbilities (GraspingCorridor a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ actionAbilityWithCost (DirectHorrorCost (toSource a) You 1)

instance RunMessage GraspingCorridor where
  runMessage msg l@(GraspingCorridor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      starvingCorridor <- getSetAsideCard Cards.starvingCorridor
      corruptedVault <- getSetAsideCard Cards.corruptedVault
      -- "Find Starving Corridor and Corrupted Vault and shuffle them into the
      -- bottom 10 cards of the encounter deck." There is no message for shuffling
      -- into the bottom N yet, so place them on the bottom of the encounter deck.
      -- TODO: shuffle into the bottom 10 specifically once a message exists.
      putCardOnBottomOfDeck iid Deck.EncounterDeck starvingCorridor
      putCardOnBottomOfDeck iid Deck.EncounterDeck corruptedVault
      GraspingCorridor <$> liftRunMessage msg attrs
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      apiaryEntrance <- getJustLocationByName "Apiary Entrance"
      farthest <- select $ FarthestLocationFromLocation apiaryEntrance Anywhere
      for_ farthest $ placeCluesUpToClueValue (attrs.ability 1)
      pure l
    _ -> GraspingCorridor <$> liftRunMessage msg attrs
