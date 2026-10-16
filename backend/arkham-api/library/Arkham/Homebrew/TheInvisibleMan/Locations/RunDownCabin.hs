{- | Run-Down Cabin -- where the notes are, once there is enough to go on.

"3 leads among all locations" counts the horror on every location, so the
criterion reads 'leadCount' rather than this location's own tokens.
-}
module Arkham.Homebrew.TheInvisibleMan.Locations.RunDownCabin (runDownCabin) where

import Arkham.Ability
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)

newtype RunDownCabin = RunDownCabin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

runDownCabin :: LocationCard RunDownCabin
runDownCabin = location RunDownCabin Cards.runDownCabin 3 (PerPlayer 2)

instance HasAbilities RunDownCabin where
  getAbilities (RunDownCabin a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "runDownCabin.notes"
      $ onlyOnce
      $ restricted
        a
        1
        ( Here
            <> HasCalculation leadCount (atLeast 3)
            <> exists (be a <> LocationWithoutClues)
        )
      $ FastAbility Free

instance RunMessage RunDownCabin where
  runMessage msg l@(RunDownCabin attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember foundGriffinsNotes
      pure l
    _ -> RunDownCabin <$> liftRunMessage msg attrs
