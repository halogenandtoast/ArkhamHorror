module Arkham.Homebrew.DarkMatter.Locations.StalagmiteForest (stalagmiteForest) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype StalagmiteForest = StalagmiteForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalagmiteForest :: LocationCard StalagmiteForest
stalagmiteForest =
  symbolLabel
    $ locationWith StalagmiteForest Cards.stalagmiteForest 2 (PerPlayer 1) (canBeFlippedL .~ True)

-- | "Forced - At the end of your turn, if you are at this location: Take 1 damage."
instance HasAbilities StalagmiteForest where
  getAbilities (StalagmiteForest a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage StalagmiteForest where
  runMessage msg l@(StalagmiteForest attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      flipToOtherSide iid attrs
      pure l
    _ -> StalagmiteForest <$> liftRunMessage msg attrs
