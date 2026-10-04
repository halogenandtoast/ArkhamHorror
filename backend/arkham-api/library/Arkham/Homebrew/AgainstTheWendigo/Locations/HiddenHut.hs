module Arkham.Homebrew.AgainstTheWendigo.Locations.HiddenHut (hiddenHut) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Stories
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HiddenHut = HiddenHut LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenHut :: LocationCard HiddenHut
hiddenHut = location HiddenHut Cards.hiddenHut 3 (PerPlayer 1)

instance HasModifiersFor HiddenHut where
  -- The unrevealed Isolated Land prints "You cannot move into the Isolated Land."
  getModifiersFor (HiddenHut a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance HasAbilities HiddenHut where
  getAbilities (HiddenHut a) =
    -- "{action}: Read the Charlie Foxtail's Destiny story card."
    extendRevealed1 a
      $ restricted a 1 (Here <> not_ (exists $ StoryIs $ Stories.charlieFoxtailsDestiny.cardCode))
      $ actionAbility

instance RunMessage HiddenHut where
  runMessage msg l@(HiddenHut attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.charlieFoxtailsDestiny
      pure l
    _ -> HiddenHut <$> liftRunMessage msg attrs
