module Arkham.Location.Cards.BlastedRuinsSunkenCircle (blastedRuinsSunkenCircle) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BlastedRuinsSunkenCircle = BlastedRuinsSunkenCircle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedRuinsSunkenCircle :: LocationCard BlastedRuinsSunkenCircle
blastedRuinsSunkenCircle = location BlastedRuinsSunkenCircle Cards.blastedRuinsSunkenCircle 2 (Static 1)

-- TODO: BACK ("Sea Floor"): when an investigator enters this location face-down
-- (enters Sea Floor), increase this and each adjacent location's flood level.
-- Requires scenario-level Sea Floor placement (TheDrownedQuarter Setup TODO).

instance HasAbilities BlastedRuinsSunkenCircle where
  getAbilities (BlastedRuinsSunkenCircle a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ TurnEnds #after You
      , groupLimit PerRound
          $ restricted a 2 (exists $ orConnected NotForMovement a <> FloodedLocation)
          $ actionAbilityWithCost (GroupResourceCost (Static 5) (be a))
      ]

instance RunMessage BlastedRuinsSunkenCircle where
  runMessage msg l@(BlastedRuinsSunkenCircle attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ IncreaseFloodLevel attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <- select $ orConnected NotForMovement attrs <> FloodedLocation
      chooseTargetM iid locations \lid -> push $ DecreaseFloodLevel lid
      pure l
    _ -> BlastedRuinsSunkenCircle <$> liftRunMessage msg attrs
