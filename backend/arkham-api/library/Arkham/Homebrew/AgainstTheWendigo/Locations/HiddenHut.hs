module Arkham.Homebrew.AgainstTheWendigo.Locations.HiddenHut (hiddenHut) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (cannotEnterUnrevealed)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgainstTheWendigo.Helpers (connectsEastOrWest, readStoryAndChoose)
import Arkham.Location.Import.Lifted

newtype HiddenHut = HiddenHut LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenHut :: LocationCard HiddenHut
hiddenHut = locationWith HiddenHut Cards.hiddenHut 3 (PerPlayer 1) connectsEastOrWest

instance HasModifiersFor HiddenHut where
  getModifiersFor (HiddenHut a) = cannotEnterUnrevealed a

instance HasAbilities HiddenHut where
  getAbilities (HiddenHut a) = extendRevealed1 a $ onlyOnce $ restricted a 1 Here actionAbility

instance RunMessage HiddenHut where
  runMessage msg l@(HiddenHut attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStoryAndChoose iid Stories.charlieFoxtailsDestiny
      pure l
    _ -> HiddenHut <$> liftRunMessage msg attrs
