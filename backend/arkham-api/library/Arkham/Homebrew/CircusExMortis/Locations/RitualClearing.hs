module Arkham.Homebrew.CircusExMortis.Locations.RitualClearing (ritualClearing) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RitualClearing = RitualClearing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualClearing :: LocationCard RitualClearing
ritualClearing = location RitualClearing Cards.ritualClearing 4 (PerPlayer 2)

-- The seal is the cost. 'SealOnInvestigatorCost' is unpayable with no moon token
-- in the bag, so the ability only needs the discover half gated.
instance HasAbilities RitualClearing where
  getAbilities (RitualClearing a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> canDiscoverCluesAt (be a))
      $ actionAbilityWithCost (SealOnInvestigatorCost moonToken)

instance RunMessage RitualClearing where
  runMessage msg l@(RitualClearing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 attrs
      pure l
    _ -> RitualClearing <$> liftRunMessage msg attrs
