module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.Gallery (gallery) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Gallery = Gallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gallery :: LocationCard Gallery
gallery = location Gallery Cards.gallery 4 (PerPlayer 2)

instance HasAbilities Gallery where
  -- "[action]: Investigate. If you succeed, discover 1 additional clue from the Auditorium."
  getAbilities (Gallery a) = extend1 a $ restricted a 1 Here actionAbility

instance RunMessage Gallery where
  runMessage msg l@(Gallery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure l
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      auditorium <- selectJust $ locationIs Cards.auditorium
      discoverAt NotInvestigate iid (attrs.ability 1) 1 auditorium
      pure l
    _ -> Gallery <$> liftRunMessage msg attrs
