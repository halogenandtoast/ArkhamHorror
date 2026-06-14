module Arkham.Location.Cards.HangingShip (hangingShip) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HangingShip = HangingShip LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangingShip :: LocationCard HangingShip
hangingShip = location HangingShip Cards.hangingShip 2 (Static 4)

instance HasAbilities HangingShip where
  getAbilities (HangingShip a) =
    extendRevealed
      a
      [ -- "After you end your turn here: test [agility] (X) where X is the number
        -- of investigators here."
        skillTestAbility $ restricted a 1 Here $ forced $ TurnEnds #after You
      , -- "[action] Each investigator here spends 1 clue: slide Hanging Ship once
        -- into an adjacent open sky."
        -- NOTE: "Each investigator here spends 1 clue" is modeled with the standard
        -- per-player group clue cost over investigators at this location. (The true
        -- cost is one clue per investigator *here*, which the engine cannot express
        -- as a static GameValue; PerPlayer is the established approximation.)
        restricted a 2 Here $ actionAbilityWithCost $ GroupClueCost (PerPlayer 1) (be a)
      ]

instance RunMessage HangingShip where
  runMessage msg l@(HangingShip attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- X = number of investigators here.
      n <- selectCount (investigatorAt attrs)
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed n)
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      -- TODO: "discard Hanging Ship to the top of the Summit deck." The Summit
      -- deck / "open sky" placeholder cards / sliding locations have no engine
      -- support yet, so the discard-to-Summit-deck is left unimplemented.
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      -- The clue cost is paid by the ability cost above.
      -- TODO: "slide Hanging Ship once into an adjacent open sky." Sliding
      -- locations into open-sky cards drawn from the Summit deck has no engine
      -- support yet, so the actual slide is left unimplemented.
      pure l
    _ -> HangingShip <$> liftRunMessage msg attrs
