module Arkham.Homebrew.DarkMatter.Locations.OlympusTelescope (olympusTelescope) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype OlympusTelescope = OlympusTelescope LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

olympusTelescope :: LocationCard OlympusTelescope
olympusTelescope = location OlympusTelescope Cards.olympusTelescope 2 (PerPlayer 1)

{- | "[action][action]: Investigate. Choose any revealed location. Investigate as
if you were at that location."
-}
instance HasAbilities OlympusTelescope where
  getAbilities (OlympusTelescope a) =
    extendRevealed1 a $ restricted a 1 Here $ doubleActionAbilityWithCost mempty

instance RunMessage OlympusTelescope where
  runMessage msg l@(OlympusTelescope attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select RevealedLocation
      chooseTargetM iid locations \lid -> do
        sid <- getRandom
        investigateEdit_ sid iid (attrs.ability 1) (setTarget lid)
      pure l
    _ -> OlympusTelescope <$> liftRunMessage msg attrs
