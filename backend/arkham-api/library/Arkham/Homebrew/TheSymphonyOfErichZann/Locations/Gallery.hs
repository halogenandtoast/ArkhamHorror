module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.Gallery (gallery) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Gallery = Gallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gallery :: LocationCard Gallery
gallery = location Gallery Cards.gallery 4 (PerPlayer 2)

instance HasAbilities Gallery where
  -- "[action]: Investigate. If you succeed, discover 1 additional clue from the Auditorium."
  getAbilities (Gallery a) =
    extend1 a
      $ campaignI18n
      $ withI18nTooltip "gallery.investigate"
      $ investigateAbility a 1 mempty Here

instance RunMessage Gallery where
  runMessage msg l@(Gallery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      withMatch (locationIs Cards.auditorium) \auditorium ->
        skillTestModifier sid (attrs.ability 1) iid (DiscoveredCluesAt auditorium 1)
      investigate sid iid (attrs.ability 1)
      pure l
    _ -> Gallery <$> liftRunMessage msg attrs
