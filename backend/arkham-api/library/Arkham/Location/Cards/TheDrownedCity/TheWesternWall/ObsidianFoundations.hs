module Arkham.Location.Cards.TheDrownedCity.TheWesternWall.ObsidianFoundations (obsidianFoundations) where

import Arkham.Ability
import Arkham.Location.CardDefs.TheDrownedCity.TheWesternWall qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheDrownedCity.TheWesternWall.Helpers (cannotEnterFromCluedLocation)
import Arkham.Treachery.CardDefs.TheDrownedCity.TheWesternWall qualified as Treacheries

newtype ObsidianFoundations = ObsidianFoundations LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianFoundations :: LocationCard ObsidianFoundations
obsidianFoundations = withXShroud $ location ObsidianFoundations Cards.obsidianFoundations 0 (Static 2)

instance HasModifiersFor ObsidianFoundations where
  getModifiersFor (ObsidianFoundations a) = cannotEnterFromCluedLocation a

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
