module Arkham.Homebrew.AgesUnwound.Locations.FulcrumOfPossibility_190 (
  fulcrumOfPossibility_190,
) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)

newtype FulcrumOfPossibility_190 = FulcrumOfPossibility_190 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The other printing of the starting location. Identical on its face; its
reverse is /The Myriad (Weapon Without Form)/, so which printing setup kept
decides whether the Myriad can ever be flipped up by act 4.
-}
fulcrumOfPossibility_190 :: LocationCard FulcrumOfPossibility_190
fulcrumOfPossibility_190 =
  symbolLabel
    $ locationWith FulcrumOfPossibility_190 Cards.fulcrumOfPossibility_190 4 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "[action]: Test [willpower] (4). If you succeed, either move to any other
location, or move another investigator to this location."
-}
instance HasAbilities FulcrumOfPossibility_190 where
  getAbilities (FulcrumOfPossibility_190 a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage FulcrumOfPossibility_190 where
  runMessage msg l@(FulcrumOfPossibility_190 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      fulcrumMove (attrs.ability 1) attrs iid
      pure l
    _ -> FulcrumOfPossibility_190 <$> liftRunMessage msg attrs
