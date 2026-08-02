module Arkham.Location.Cards.HangingShip (hangingShip) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Window (getBatchId)

newtype HangingShip = HangingShip LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangingShip :: LocationCard HangingShip
hangingShip = location HangingShip Cards.hangingShip 2 (Static 4)

instance HasAbilities HangingShip where
  getAbilities (HangingShip a) =
    if a.revealed
      then
        extendRevealed
          a
          [ -- "After you end your turn here: test [agility] (X) where X is the number
            -- of investigators here."
            skillTestAbility $ restricted a 1 Here $ forced $ TurnEnds #after You
          , -- "[action] Each investigator at this location spends 1 clue: slide
            -- Hanging Ship once into an adjacent open sky."
            restricted a 2 (Here <> exists (isOpenSky <> LocationWithDistanceFromAtMost 1 (be a) Anywhere))
              $ actionAbilityWithCost
              $ SameLocationGroupClueCost (PerPlayer 1) (be a)
          ]
      else extendUnrevealed1 a (summitEntry a 9)

instance RunMessage HangingShip where
  runMessage msg l@(HangingShip attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- X = number of investigators here.
      n <- selectCount (investigatorAt attrs)
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed n)
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      -- "Discard Hanging Ship (to the top of the Summit deck)." Anyone still
      -- aboard is caught by the agenda's location-leaves-play Forced.
      returnToSummitDeck [attrs.id]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "Slide Hanging Ship once into an adjacent open sky": the ship and the
      -- sky trade places, so no gap opens and nothing leaves play.
      openSkies <- getAdjacentOpenSky attrs.id
      chooseTargetM iid openSkies $ swapGridPositions attrs.id
      pure l
    _ -> HangingShip <$> liftRunMessage msg attrs
