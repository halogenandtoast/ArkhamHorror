module Arkham.Homebrew.AgesUnwound.Locations.BanksOfTheNile_078 (banksOfTheNile_078) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BanksOfTheNile_078 = BanksOfTheNile_078 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Banks of the Nile/, the printing that buries you in sand instead.
banksOfTheNile_078 :: LocationCard BanksOfTheNile_078
banksOfTheNile_078 =
  locationWith BanksOfTheNile_078 Cards.banksOfTheNile_078 2 (PerPlayer 2)
    $ connectsToL
    .~ ringConnections

{- | "Forced - At the end of your turn: Test [intellect] (3). If you fail, search
the encounter deck and discard pile for a copy of Sandstorm and draw it."
-}
instance HasAbilities BanksOfTheNile_078 where
  getAbilities (BanksOfTheNile_078 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage BanksOfTheNile_078 where
  runMessage msg l@(BanksOfTheNile_078 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      findAndDrawEncounterCard iid (cardIs Treacheries.sandstorm)
      pure l
    _ -> BanksOfTheNile_078 <$> liftRunMessage msg attrs
