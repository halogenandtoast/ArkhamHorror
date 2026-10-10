module Arkham.Homebrew.AgesUnwound.Locations.FulcrumOfPossibility_189 (
  fulcrumOfPossibility_189,
) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)

newtype FulcrumOfPossibility_189 = FulcrumOfPossibility_189 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The starting location, and the first of the eight paired printings. Its
reverse face is the @[[Paradox]]@ /Fulcrum of Possibility (Heart of Corruption)/
rather than an unrevealed location side, so it enters play revealed.
-}
fulcrumOfPossibility_189 :: LocationCard FulcrumOfPossibility_189
fulcrumOfPossibility_189 =
  symbolLabel
    $ locationWith FulcrumOfPossibility_189 Cards.fulcrumOfPossibility_189 4 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "[action]: Test [willpower] (4). If you succeed, either move to any other
location, or move another investigator to this location."
-}
instance HasAbilities FulcrumOfPossibility_189 where
  getAbilities (FulcrumOfPossibility_189 a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage FulcrumOfPossibility_189 where
  runMessage msg l@(FulcrumOfPossibility_189 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      fulcrumMove (attrs.ability 1) attrs iid
      pure l
    _ -> FulcrumOfPossibility_189 <$> liftRunMessage msg attrs
