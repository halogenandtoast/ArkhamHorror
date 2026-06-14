module Arkham.Location.Cards.ObsidianFoundations (obsidianFoundations) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Treachery.Cards qualified as Treacheries

newtype ObsidianFoundations = ObsidianFoundations LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianFoundations :: LocationCard ObsidianFoundations
obsidianFoundations = location ObsidianFoundations Cards.obsidianFoundations 0 (Static 2)

instance HasAbilities ObsidianFoundations where
  getAbilities (ObsidianFoundations a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage ObsidianFoundations where
  runMessage msg l@(ObsidianFoundations attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      drawCard iid =<< getSetAsideCard Treacheries.seafloorFrieze
      pure l
    _ -> ObsidianFoundations <$> liftRunMessage msg attrs
