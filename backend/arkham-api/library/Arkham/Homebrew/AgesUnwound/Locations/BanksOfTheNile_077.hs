module Arkham.Homebrew.AgesUnwound.Locations.BanksOfTheNile_077 (banksOfTheNile_077) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BanksOfTheNile_077 = BanksOfTheNile_077 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Banks of the Nile/. Both printings hold two clues per investigator behind
the lowest shroud in the ring, and both are where Eager Sphinx spawns.
-}
banksOfTheNile_077 :: LocationCard BanksOfTheNile_077
banksOfTheNile_077 =
  locationWith BanksOfTheNile_077 Cards.banksOfTheNile_077 2 (PerPlayer 2)
    $ connectsToL
    .~ ringConnections

{- | "Forced - At the end of your turn: Test [agility] (3). For each point you
fail by, lose 1 resource."

A flat count, not a per-point loop: nothing is chosen or resolved per point.
-}
instance HasAbilities BanksOfTheNile_077 where
  getAbilities (BanksOfTheNile_077 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage BanksOfTheNile_077 where
  runMessage msg l@(BanksOfTheNile_077 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      loseResources iid (attrs.ability 1) n
      pure l
    _ -> BanksOfTheNile_077 <$> liftRunMessage msg attrs
