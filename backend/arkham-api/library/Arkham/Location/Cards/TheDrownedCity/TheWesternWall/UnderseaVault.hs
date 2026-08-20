module Arkham.Location.Cards.TheDrownedCity.TheWesternWall.UnderseaVault (underseaVault) where

import Arkham.Ability
import Arkham.Helpers.Story (readStory)
import Arkham.Location.CardDefs.TheDrownedCity.TheWesternWall qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheWesternWall.Helpers (locationLevel)
import Arkham.Story.CardDefs.TheDrownedCity.TheWesternWall qualified as Stories

newtype UnderseaVault = UnderseaVault LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaVault :: LocationCard UnderseaVault
underseaVault =
  locationWith UnderseaVault Cards.underseaVault 5 (Static 1) (canBeFlippedL .~ True)

instance HasAbilities UnderseaVault where
  getAbilities (UnderseaVault a) =
    extendRevealed1 a $ onlyOnce $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage UnderseaVault where
  runMessage msg l@(UnderseaVault attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let lvl = maybe 0 locationLevel (locationPosition attrs)
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed lvl)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when (locationCanBeFlipped attrs) $ flipOver iid attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Stories.theUnderseaVault
      pure . UnderseaVault $ attrs & canBeFlippedL .~ False
    _ -> UnderseaVault <$> liftRunMessage msg attrs
