module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.StageHall (stageHall) where

import Arkham.Ability
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype StageHall = StageHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stageHall :: LocationCard StageHall
stageHall = location StageHall Cards.stageHall 3 (Static 1)

instance HasAbilities StageHall where
  -- "[action]: Shuffle the encounter discard pile into the encounter deck."
  getAbilities (StageHall a) = extend1 a $ restricted a 1 Here actionAbility

instance RunMessage StageHall where
  runMessage msg l@(StageHall attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      shuffleEncounterDiscardBackIn
      pure l
    _ -> StageHall <$> liftRunMessage msg attrs
